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
				return gitDisjointPlugin(), nil
			}},
		},
	})
}

func gitDisjointPlugin() schema.Plugin {
	return schema.Plugin{
		Name: "git-disjoint",
		Platform: schema.PlatformInfo{
			Name:     "git-disjoint",
			Homepage: sdk.URL("https://github.com/ericcrosson/git-disjoint"),
		},
		Credentials: []schema.CredentialType{
			personalAccessToken(),
		},
		Executables: []schema.Executable{
			gitDisjointCLI(),
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
				MarkdownDescription: "Token used to authenticate to GitHub for git-disjoint.",
				Secret:              true,
				Composition: &schema.ValueComposition{
					Length: 40,
					Prefix: "ghp_",
					Charset: schema.Charset{
						Uppercase: true,
						Lowercase: true,
						Digits:    true,
					},
				},
			},
		},
		DefaultProvisioner: provision.EnvVars(map[string]sdk.FieldName{
			"GITHUB_TOKEN": fieldname.Token,
		}),
		Importer: importer.TryAll(
			importer.TryEnvVarPair(map[string]sdk.FieldName{
				"GITHUB_TOKEN": fieldname.Token,
			}),
		),
	}
}

func gitDisjointCLI() schema.Executable {
	return schema.Executable{
		Name:    "git-disjoint",
		Runs:    []string{"git-disjoint-unwrapped"},
		DocsURL: sdk.URL("https://github.com/ericcrosson/git-disjoint"),
		NeedsAuth: needsauth.IfAll(
			needsauth.NotForHelpOrVersion(),
			needsauth.NotWithoutArgs(),
		),
		Uses: []schema.CredentialUsage{
			{
				Name: credname.PersonalAccessToken,
			},
		},
	}
}
