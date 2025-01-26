{
  lib,
  buildGoModule,
  fetchFromGitHub,
  makeWrapper,
  stdenv,
}:
buildGoModule rec {
  pname = "aws-saml";
  version = "0.1.0-rc.3";

  src = fetchFromGitHub {
    owner = "joshdk";
    repo = "aws-saml";
    rev = "v${version}";
    sha256 = "sha256-EM+dfBPHKcs3/Uo1nPet3vhEKYwTRHS+4wFerxx8FHk=";
  };

  vendorHash = "sha256-fpVicjHomS9ZWYfjOJhqxbr5F+HLKshZvLgcG0D+KZQ=";

  nativeBuildInputs = [makeWrapper];

  postFixup = lib.optionalString stdenv.isDarwin ''
    wrapProgram $out/bin/aws-saml \
      --prefix PATH : /usr/bin
  '';

  meta = with lib; {
    description = "🔏 Generate AWS credentials from a SAML IdP login";
    homepage = "https://github.com/joshdk/aws-saml";
    license = licenses.mit;
    maintainers = [];
  };
}
