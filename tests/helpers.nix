{lib}: {
  assertEq = name: actual: expected:
    if actual == expected
    then true
    else builtins.throw "Test '${name}' failed: expected ${builtins.toJSON expected}, got ${builtins.toJSON actual}";

  assertHasAttr = name: attrset: attr:
    if builtins.hasAttr attr attrset
    then true
    else builtins.throw "Test '${name}' failed: attribute '${attr}' not found in ${builtins.toJSON attrset}";

  assertNotHasAttr = name: attrset: attr:
    if !builtins.hasAttr attr attrset
    then true
    else builtins.throw "Test '${name}' failed: attribute '${attr}' should not exist";

  # lib.hasInfix signature: needle -> haystack
  assertContains = name: haystack: needle:
    if lib.hasInfix needle haystack
    then true
    else builtins.throw "Test '${name}' failed: expected string to contain ${builtins.toJSON needle}";
}
