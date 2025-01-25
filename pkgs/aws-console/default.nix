{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:
buildGoModule {
  pname = "aws-console";
  version = "0.5.0-rc.0";

  src = fetchFromGitHub {
    owner = "joshdk";
    repo = "aws-console";
    rev = "0fd2d9dad0de846a520be0401b95b16da66f8e05";
    hash = "sha256-T1H2FGcc53NsnZFy8w3cB61MLjro2vXhcagx32Id16g=";
  };

  vendorHash = "sha256-Gf9qxxu58isR+1LeH3v6l1Ics+WQqUgrYz6Z6t6lzBc=";

  meta = with lib; {
    description = "🔗 Generate a temporary login URL for the AWS Console";
    homepage = "https://github.com/joshdk/aws-console";
    license = licenses.mit;
    maintainers = [];
  };
}
