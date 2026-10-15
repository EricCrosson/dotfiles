//! Rift hotkey bridge.
//!
//! Rift 0.5.5 registered its global-hotkey event tap at the macOS *session*
//! event point; 0.6.x moved the tap to the *HID* event point. Hardware
//! keystrokes enter the event stream at the HID point and are unaffected, but
//! keystrokes synthesized by other processes with `CGEventPost` at the
//! session point (Logitech G HUB macro playback, for example) enter below
//! the HID tap, so rift never sees them: the hotkey stops firing and the
//! keystroke lands in the focused application (Option+Comma types "≤" on a
//! US layout).
//!
//! This daemon restores the old behavior without patching rift: it listens
//! at the annotated-session point (which sees session-posted events),
//! consumes matching keydowns, and re-posts them at the HID point, where
//! rift's own tap dispatches and consumes them as if they came from the
//! keyboard. Re-posted events are marked with `CGEventSourceUserData`; if
//! nothing consumes the mark downstream (rift is not running), the event is
//! passed through to the focused application unchanged.

use std::ffi::c_void;
use std::ptr;
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

type CFAllocatorRef = *mut c_void;
type CFMachPortRef = *mut c_void;
type CFRunLoopRef = *mut c_void;
type CFRunLoopSourceRef = *mut c_void;
type CFStringRef = *const c_void;
type CGEventRef = *mut c_void;

// CGEventTapLocation
const CG_HID_EVENT_TAP: u32 = 0;
const CG_SESSION_EVENT_TAP: u32 = 1;
const CG_ANNOTATED_SESSION_EVENT_TAP: u32 = 2;
// CGEventTapPlacement
const CG_HEAD_INSERT_EVENT_TAP: u32 = 0;
// CGEventTapOptions
const CG_EVENT_TAP_OPTION_DEFAULT: u32 = 0;
// CGEventType
const CG_EVENT_KEY_DOWN: u64 = 10;
// CGEventField (CF_ENUM(uint32_t, CGEventField) in CGEventTypes.h)
const CG_KEYBOARD_EVENT_KEYCODE: u32 = 9;
const CG_EVENT_SOURCE_USER_DATA: u32 = 42; // kCGEventSourceUserData

// CGEventFlags (mask bits; higher side-specific bits are ignored by masking)
const MASK_SHIFT: u64 = 0x0002_0000;
const MASK_CONTROL: u64 = 0x0004_0000;
const MASK_ALTERNATE: u64 = 0x0008_0000;
const MASK_COMMAND: u64 = 0x0010_0000;

// Marks our own re-posts so the callback never relays an event twice.
const BRIDGE_MARKER: i64 = 0x7269_6674; // 'rift'

#[derive(Clone, Copy, Debug)]
struct Relay {
    keycode: i64,
    flags: u64,
}

static RELAYS: OnceLock<Vec<Relay>> = OnceLock::new();

fn now_ms() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or(0)
}

unsafe extern "C" fn tap_callback(
    _proxy: *mut c_void,
    _tap_type: u32,
    event: CGEventRef,
    _user_info: *mut c_void,
) -> CGEventRef {
    if event.is_null() {
        return ptr::null_mut();
    }
    let keycode = CGEventGetIntegerValueField(event, CG_KEYBOARD_EVENT_KEYCODE);
    if CGEventGetIntegerValueField(event, CG_EVENT_SOURCE_USER_DATA) == BRIDGE_MARKER {
        // Our own re-post came back after nothing consumed it upstream
        // (rift is not running): deliver it to the focused application as
        // before.
        eprintln!("[{}] marked passthrough keycode={}", now_ms(), keycode);
        return event;
    }
    let flags = CGEventGetFlags(event);
    let relays = RELAYS.get().map(|v| v.as_slice()).unwrap_or(&[]);
    if let Some(relay) = relays
        .iter()
        .find(|r| keycode == r.keycode && (flags & r.flags) == r.flags)
    {
        eprintln!(
            "[{}] relaying keycode={} flags=0x{:x} -> HID",
            now_ms(),
            keycode,
            relay.flags
        );
        CGEventSetIntegerValueField(event, CG_EVENT_SOURCE_USER_DATA, BRIDGE_MARKER);
        CGEventPost(CG_HID_EVENT_TAP, event);
        // Consumed: the focused application never sees the original.
        return ptr::null_mut();
    }
    event
}

extern "C" {
    fn CGEventTapCreate(
        tap: u32,
        place: u32,
        options: u32,
        events_of_interest: u64,
        callback: unsafe extern "C" fn(*mut c_void, u32, CGEventRef, *mut c_void) -> CGEventRef,
        user_info: *mut c_void,
    ) -> CFMachPortRef;
    fn CGEventTapEnable(tap: CFMachPortRef, enable: bool);
    fn CGEventPost(tap: u32, event: CGEventRef);
    fn CGEventGetIntegerValueField(event: CGEventRef, field: u32) -> i64;
    fn CGEventSetIntegerValueField(event: CGEventRef, field: u32, value: i64);
    fn CGEventGetFlags(event: CGEventRef) -> u64;
    fn CFMachPortCreateRunLoopSource(
        allocator: CFAllocatorRef,
        port: CFMachPortRef,
        order: i64,
    ) -> CFRunLoopSourceRef;
    fn CFRunLoopGetMain() -> CFRunLoopRef;
    fn CFRunLoopAddSource(run_loop: CFRunLoopRef, source: CFRunLoopSourceRef, mode: CFStringRef);
    fn CFRunLoopRun();
    static kCFRunLoopCommonModes: CFStringRef;
}

fn parse_modifier(token: &str) -> Option<u64> {
    match token.to_ascii_lowercase().as_str() {
        "alt" | "option" | "opt" => Some(MASK_ALTERNATE),
        "shift" => Some(MASK_SHIFT),
        "ctrl" | "control" => Some(MASK_CONTROL),
        "cmd" | "command" | "meta" => Some(MASK_COMMAND),
        _ => None,
    }
}

fn parse_relay(spec: &str) -> Result<Relay, String> {
    let (keycode_raw, modifiers_raw) = spec
        .split_once(':')
        .ok_or_else(|| format!("relay `{spec}` must look like `43:alt`"))?;
    let keycode: i64 = keycode_raw
        .trim()
        .parse()
        .map_err(|_| format!("relay `{spec}`: invalid keycode `{keycode_raw}`"))?;
    let mut flags = 0u64;
    for token in modifiers_raw.split(',') {
        let token = token.trim();
        if token.is_empty() {
            continue;
        }
        flags |= parse_modifier(token)
            .ok_or_else(|| format!("relay `{spec}`: unknown modifier `{token}` (alt|shift|ctrl|cmd)"))?;
    }
    Ok(Relay { keycode, flags })
}

fn usage() -> ! {
    eprintln!("usage: rift-hotkey-bridge [--relay KEYCODE:MODIFIERS]...");
    eprintln!("  MODIFIERS: alt|shift|ctrl|cmd, comma-separated (default: 43:alt)");
    std::process::exit(2);
}

fn main() {
    let mut relays: Vec<Relay> = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        if let Some(spec) = arg.strip_prefix("--relay=") {
            match parse_relay(spec) {
                Ok(relay) => relays.push(relay),
                Err(err) => {
                    eprintln!("{err}");
                    usage();
                }
            }
            continue;
        }
        match arg.as_str() {
            "--relay" => {
                let Some(spec) = args.next() else {
                    eprintln!("--relay needs a KEYCODE:MODIFIERS value");
                    usage();
                };
                match parse_relay(&spec) {
                    Ok(relay) => relays.push(relay),
                    Err(err) => {
                        eprintln!("{err}");
                        usage();
                    }
                }
            }
            _ => usage(),
        }
    }
    if relays.is_empty() {
        relays.push(parse_relay("43:alt").expect("built-in default relay parses"));
    }
    RELAYS.set(relays.clone()).expect("relays are set once");

    let events_of_interest = 1u64 << CG_EVENT_KEY_DOWN;
    unsafe {
        let tap = CGEventTapCreate(
            CG_ANNOTATED_SESSION_EVENT_TAP,
            CG_HEAD_INSERT_EVENT_TAP,
            CG_EVENT_TAP_OPTION_DEFAULT,
            events_of_interest,
            tap_callback,
            ptr::null_mut(),
        );
        if tap.is_null() {
            eprintln!("CGEventTapCreate failed (accessibility permission required)");
            std::process::exit(1);
        }
        CGEventTapEnable(tap, true);
        let source = CFMachPortCreateRunLoopSource(ptr::null_mut(), tap, 0);
        if source.is_null() {
            eprintln!("CFMachPortCreateRunLoopSource failed");
            std::process::exit(1);
        }
        CFRunLoopAddSource(CFRunLoopGetMain(), source, kCFRunLoopCommonModes);
        eprintln!(
            "[{}] listening at the annotated-session event tap; relays: {:?}",
            now_ms(),
            relays
        );
        CFRunLoopRun();
    }
}
