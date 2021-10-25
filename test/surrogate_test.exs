defmodule SurrogateTest do
  use ExUnit.Case
  doctest Surrogate

  test "greets the world" do
    assert Surrogate.hello() == :world
  end
end
