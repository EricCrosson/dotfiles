package main

import (
	"github.com/1Password/shell-plugins/sdk"
	"github.com/1Password/shell-plugins/sdk/importer"
	"github.com/1Password/shell-plugins/sdk/needsauth"
	"github.com/1Password/shell-plugins/sdk/provision"
	"github.com/1Password/shell-plugins/sdk/rpc/proto"
	"github.com/1Password/shell-plugins/sdk/rpc/server"
	"github.com/1Password/shell-plugins/sdk/schema"
	"github.com/1Password/shell-plugins/sdk/schema/credname"
	"github.com/1Password/shell-plugins/sdk/schema/fieldname"
	"github.com/hashicorp/go-plugin"
)

func main() {
	plugin.Serve(&plugin.ServeConfig{
		HandshakeConfig: plugin.HandshakeConfig{
			ProtocolVersion:  proto.Version,
			MagicCookieKey:   proto.MagicCookieKey,
			MagicCookieValue: proto.MagicCookieValue,
		},
		Plugins: plugin.PluginSet{
			"plugin": &server.RPCPlugin{RPCPlugin: func() (schema.Plugin, error) {
				return ghPlugin(), nil
			}},
		},
	})
}

func ghPlugin() schema.Plugin {
	return schema.Plugin{
		Name: "gh",
		Platform: schema.PlatformInfo{
			Name:     "GitHub CLI",
			Homepage: sdk.URL("https://cli.github.com"),
		},
		Credentials: []schema.CredentialType{
			personalAccessToken(),
		},
		Executables: []schema.Executable{
			ghCLI(),
		},
	}
}

func personalAccessToken() schema.CredentialType {
	return schema.CredentialType{
		Name:          credname.PersonalAccessToken,
		DocsURL:       sdk.URL("https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token"),
		ManagementURL: sdk.URL("https://github.com/settings/tokens"),
		Fields: []schema.CredentialField{
			{
				Name:                fieldname.Token,
				MarkdownDescription: "Token used to authenticate to GitHub.",
				Secret:              true,
				Composition: &schema.ValueComposition{
					Charset: schema.Charset{
						Uppercase: true,
						Lowercase: true,
						Digits:    true,
						Symbols:   true,
					},
				},
			},
		},
		DefaultProvisioner: provision.EnvVars(map[string]sdk.FieldName{
			"GH_TOKEN": fieldname.Token,
		}),
		Importer: importer.TryAll(
			importer.TryEnvVarPair(map[string]sdk.FieldName{
				"GH_TOKEN": fieldname.Token,
			}),
		),
	}
}

func ghCLI() schema.Executable {
	return schema.Executable{
		Name:    "GitHub CLI",
		Runs:    []string{"gh-unwrapped"},
		DocsURL: sdk.URL("https://cli.github.com/manual/"),
		NeedsAuth: needsauth.IfAll(
			needsauth.NotForHelpOrVersion(),
			needsauth.NotWhenContainsArgs("auth"),
		),
		Uses: []schema.CredentialUsage{
			{
				Name: credname.PersonalAccessToken,
			},
		},
	}
}
