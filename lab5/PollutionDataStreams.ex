defmodule PollutionDataStreams do
    def makeMap([day, hour, coord1, coord2, level]) do
        %{:datetime => {
            day |> String.split("-") |> Enum.map(fn(s)->Integer.parse(s) |> elem(0) end) |> Enum.reverse |> :erlang.list_to_tuple, 
            hour |> String.split(":") |> Enum.map(fn(s)->Integer.parse(s) |> elem(0) end) |> :erlang.list_to_tuple
        },
        :location => {Float.parse(coord1) |> elem(0), Float.parse(coord2) |> elem(0)},
        :pollution_level => Integer.parse(level) |> elem(0)}
    end

    def getLocation(dict) do
        dict[:location]
    end

    def importLinesFromCSV(filePath) do
        File.stream!(filePath) |> Stream.map(&String.trim(&1)) |>Stream.map(&String.split(&1, ",")) |> Stream.map(&makeMap(&1))
    end

    def findUniqueStations(data) do
        data |> Stream.uniq_by(&getLocation(&1)) |> Stream.map(&getLocation(&1))
    end

    def loadStations(filePath) do
        importLinesFromCSV(filePath) |> findUniqueStations |> 
        Enum.reduce(0, fn(el, acc) -> :pollution_gen_server.addStation("Station#{acc}", el) 
        acc + 1 end)
    end

    def loadData(filePath) do
        importLinesFromCSV(filePath) |> 
        Enum.reduce(nil, fn(el, _)-> :pollution_gen_server.addValue(el[:location], "PM10", el[:datetime], el[:pollution_level])
        nil end)
    end   
end