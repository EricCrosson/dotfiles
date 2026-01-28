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
				return jiraPlugin(), nil
			}},
		},
	})
}

func jiraPlugin() schema.Plugin {
	return schema.Plugin{
		Name: "jira",
		Platform: schema.PlatformInfo{
			Name:     "Jira CLI",
			Homepage: sdk.URL("https://github.com/ankitpokhrel/jira-cli"),
		},
		Credentials: []schema.CredentialType{
			apiToken(),
		},
		Executables: []schema.Executable{
			jiraCLI(),
		},
	}
}

func apiToken() schema.CredentialType {
	return schema.CredentialType{
		Name:          credname.APIToken,
		DocsURL:       sdk.URL("https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/"),
		ManagementURL: sdk.URL("https://id.atlassian.com/manage-profile/security/api-tokens"),
		Fields: []schema.CredentialField{
			{
				Name:                fieldname.Token,
				MarkdownDescription: "Token used to authenticate to Jira.",
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
			"JIRA_API_TOKEN": fieldname.Token,
		}),
		Importer: importer.TryAll(
			importer.TryEnvVarPair(map[string]sdk.FieldName{
				"JIRA_API_TOKEN": fieldname.Token,
			}),
		),
	}
}

func jiraCLI() schema.Executable {
	return schema.Executable{
		Name:    "Jira CLI",
		Runs:    []string{"jira-unwrapped"},
		DocsURL: sdk.URL("https://github.com/ankitpokhrel/jira-cli"),
		NeedsAuth: needsauth.IfAll(
			needsauth.NotForHelpOrVersion(),
			needsauth.NotWithoutArgs(),
		),
		Uses: []schema.CredentialUsage{
			{
				Name: credname.APIToken,
			},
		},
	}
}
