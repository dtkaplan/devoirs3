# devoirs3

`devoirs3` is an R package for creating self-contained HTML assignments from Quarto documents and collecting student responses. The package is under development; see `design-spec.md` in the source repository for the current design contract.

For authoring multiple-choice answers in Quarto, the [`devoirs_choices` chunk
engine](man/devoirs_choices.Rd) lets you declare choices as YAML and pass them
to `devoirs_selection()`. See the help topic `?devoirs_choices` for the syntax,
defaults, and a complete example.
