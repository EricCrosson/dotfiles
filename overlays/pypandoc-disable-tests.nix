# This overlay disables tests for the pypandoc Python package.
#
# Context: Open WebUI depends on pypandoc, but the package's tests fail during
# the build process. These failures don't affect the functionality needed by
# Open WebUI, but they prevent successful builds.
#
# This overlay overrides the pypandoc package to skip its test suite during
# the build process, allowing Open WebUI to build successfully without
# having to modify its dependencies directly.
_final: prev: {
  python3Packages =
    prev.python3Packages
    // {
      pypandoc = prev.python3Packages.pypandoc.overridePythonAttrs (_prev: {
        doCheck = false;
      });
    };
}
