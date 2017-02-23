defmodule PlymioAst.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [app: :plymio_ast,
     version: @version,
     description: description(),
     package: package(),
     source_url: "https://github.com/ianrumford/plymio_ast",
     homepage_url: "https://github.com/ianrumford/plymio_ast",
     docs: [extras: ["./README.md", "./CHANGELOG.md"]],
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:harnais, "~> 0.1.0", only: :test},
      {:ex_doc, "~> 0.14.5", only: :dev}
    ]
  end

  defp package do
    [maintainers: ["Ian Rumford"],
     files: ["lib", "mix.exs", "README*", "LICENSE*", "CHANGELOG*"],
     licenses: ["MIT"],
     links: %{github: "https://github.com/ianrumford/plymio_ast"}]
  end

  defp description do
    """
    plymio_ast: Utility Functions for ASTs (Quoted Forms)
    """
  end

end
