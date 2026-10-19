"""MLflow request-auth provider for deployments behind Cloudflare Access.

MLflow deployments gated by Cloudflare Access require a valid
``cf-access-jwt-assertion`` header on requests that reach the origin. This
provider mints that JWT with the ``cloudflared`` CLI (whose token cache lives
under ``~/.cloudflared``) and attaches it to every outgoing MLflow request.

Activate it with ``MLFLOW_TRACKING_AUTH=cloudflare-access``.

Environment:
    MLFLOW_CF_ACCESS_APP: Access application URL whose JWT is minted
        (default: https://mlflow.bitgo-prod.com).
    MLFLOW_CF_ACCESS_CLOUDFLARED: cloudflared executable (default: resolved
        from PATH).
"""

from __future__ import annotations

import base64
import json
import os
import subprocess
import threading
import time

import requests

from mlflow.tracking.request_auth.abstract_request_auth_provider import (
    RequestAuthProvider,
)

PROVIDER_NAME = "cloudflare-access"
JWT_HEADER = "CF-Access-Jwt-Assertion"
DEFAULT_APP = "https://mlflow.bitgo-prod.com"

# Re-mint the JWT this long before it expires so an in-flight request never
# presents a token the edge has just stopped accepting.
_REFRESH_MARGIN_SECONDS = 300

_lock = threading.Lock()
_cached: tuple[str, float] | None = None  # (jwt, expiry epoch seconds)


def _jwt_expiry(jwt: str) -> float:
    """Return the ``exp`` claim of a JWT, or 0 when it cannot be read."""
    try:
        payload = jwt.split(".")[1]
        payload += "=" * (-len(payload) % 4)
        return float(json.loads(base64.urlsafe_b64decode(payload)).get("exp", 0))
    except (IndexError, ValueError, TypeError, KeyError):
        return 0.0


def _mint_jwt() -> tuple[str, float]:
    app = os.environ.get("MLFLOW_CF_ACCESS_APP", DEFAULT_APP)
    cloudflared = os.environ.get("MLFLOW_CF_ACCESS_CLOUDFLARED", "cloudflared")
    try:
        proc = subprocess.run(
            [cloudflared, "access", "token", "--app", app],
            capture_output=True,
            text=True,
            timeout=30,
        )
    except OSError as exc:
        raise RuntimeError(
            f"cannot run {cloudflared!r}: {exc}. "
            "Install cloudflared or point MLFLOW_CF_ACCESS_CLOUDFLARED at it."
        ) from exc
    except subprocess.TimeoutExpired as exc:
        raise RuntimeError(f"{cloudflared!r} access token timed out for {app}") from exc

    if proc.returncode != 0:
        detail = proc.stderr.strip() or proc.stdout.strip() or f"exit code {proc.returncode}"
        raise RuntimeError(
            f"cloudflared access token --app {app} failed: {detail}. "
            f"Run `cloudflared access login {app}` to authenticate."
        )

    jwt = proc.stdout.strip()
    expiry = _jwt_expiry(jwt)
    if not jwt or expiry <= time.time():
        raise RuntimeError(
            f"cloudflared returned no usable JWT for {app}. "
            f"Run `cloudflared access login {app}` to authenticate."
        )
    return jwt, expiry


def _jwt() -> str:
    global _cached
    with _lock:
        if _cached and _cached[1] - time.time() > _REFRESH_MARGIN_SECONDS:
            return _cached[0]
        _cached = _mint_jwt()
        return _cached[0]


class _CloudflareAccessAuth(requests.auth.AuthBase):
    def __call__(self, request: requests.PreparedRequest) -> requests.PreparedRequest:
        request.headers[JWT_HEADER] = _jwt()
        return request


class CloudflareAccessAuthProvider(RequestAuthProvider):
    """Attach the Cloudflare Access JWT to every MLflow request."""

    def get_name(self) -> str:
        return PROVIDER_NAME

    def get_auth(self) -> requests.auth.AuthBase:
        return _CloudflareAccessAuth()
