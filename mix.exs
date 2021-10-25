defmodule Surrogate.MixProject do
  use Mix.Project

  def project do
    [
      app: :surrogate,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :sasl, :inets, :mnesia, :crypto, :public_key, :ssl],
      mod: {Surrogate.Application, []},
      registered: [:surrogate_log, :proxyconf, :surrogate_stats, :listener_sup, :listener_helper],
      env: [proxy_conf: "conf/proxy.conf", proxy_packup_conf: "conf/proxy-backup.conf"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
