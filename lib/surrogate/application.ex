defmodule Surrogate.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: Surrogate.Worker.start_link(arg)
      # {Surrogate.Worker, arg}
      %{
        id: :surrogate_log,
        start: {:surrogate_log, :start_link, []}
      },
      %{
        id: :proxyconf,
        start: {:proxyconf, :start_link, []}
      },
      %{
        id: :surrogate_stats,
        start: {:surrogate_stats, :start_link, []}
      },
      %{
        id: :listener_sup,
        start: {:listener_sup, :start_link, []}
      },
      %{
        id: :listener_helper,
        start: {:listener_helper, :start_link, []}
      }
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Surrogate.Supervisor]
    Supervisor.start_link(children, opts)

  end
end
