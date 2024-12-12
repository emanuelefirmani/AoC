module AdventOfCode.D12.Resolution

open System
open AdventOfCode

type Plot = { plant: char; y: int; x: int }
type Area = Plot seq

let isAdiacent plot1 plot2 =
    let diffY = Math.Abs (plot1.y - plot2.y)
    let diffX = Math.Abs (plot1.x - plot2.x)
    diffX + diffY = 1

let isConnectedToArea plot area =
    area
    |> Seq.exists (isAdiacent plot) 
let isNotConnectedToArea plot area = not (isConnectedToArea plot area)

let aggregateRegions (areas: Area seq) plot=
    let connectedAreas =
        areas
        |> Seq.filter (isConnectedToArea plot)
    let notConnectedAreas =
        areas
        |> Seq.filter (isNotConnectedToArea plot)
    let newArea: Area =
        if Seq.length connectedAreas = 0 then
            [plot]
        else
            let newAreas = connectedAreas |> Seq.concat
            Seq.append [plot] newAreas

    Seq.append notConnectedAreas [newArea]
    

let private findRegions (plots: Plot seq) =
    plots |> Seq.fold aggregateRegions []

let calculateArea area =
    area
    |> Seq.length

let calculatePerimeterN (map: Plot array array) (plot: Plot) =
    if plot.y = 0 then
        1
    else if (map[plot.y - 1][plot.x]).plant = plot.plant then
        0
    else
        1
let calculatePerimeterS (map: Plot array array) (plot: Plot) =
    if plot.y = map.Length - 1 then
        1
    else if (map[plot.y + 1][plot.x]).plant = plot.plant then
        0
    else
        1
let calculatePerimeterE (map: Plot array array) (plot: Plot) =
    if plot.x = map[plot.x].Length - 1 then
        1
    else if (map[plot.y][plot.x + 1]).plant = plot.plant then
        0
    else
        1
let calculatePerimeterW (map: Plot array array) (plot: Plot) =
    if plot.x = 0 then
        1
    else if (map[plot.y][plot.x - 1]).plant = plot.plant then
        0
    else
        1
let calculatePerimeterOfPlot map plot =
    let p =
        calculatePerimeterN map plot +
        calculatePerimeterS map plot +
        calculatePerimeterE map plot +
        calculatePerimeterW map plot
    p

let calculatePerimeter (map: Plot array array) (area: Area) =
    area
    |> Seq.map (calculatePerimeterOfPlot map)
    |> Seq.sum

let calculateFencePrice  (map: Plot array array) (areas: Area seq) =
    areas
    |> Seq.map (fun a -> (calculateArea a) * (calculatePerimeter map a))
    |> Seq.map decimal
    |> Seq.sum

let findAllRegions (plots: Plot array array) =
    let f (_,l) = findRegions l

    plots
    |> Array.collect id
    |> Seq.groupBy _.plant
    |> Seq.map f
    |> Seq.collect id

let parseLine y (line: string) =
    line.ToCharArray()
    |> Array.mapi (fun x c -> { plant = c; y = y; x = x })

let parse input : Plot array array = input |> FileSplitter.splitInLines |> Array.mapi parseLine
