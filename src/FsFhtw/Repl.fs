module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type State = Domain.State
type Market= Domain.EnergyMarket
type Player = Domain.Player
type Participant= MarketParticipant



open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let printMarket (market : Domain.EnergyMarket) =
    printfn "Storage Capacity: %A" market.storageCapacity
    printfn "Exess Energy: %A" market.excessEnergy
    printfn "Max Price: %A" market.maxEnergyPrice
    printfn "Min Price: %A" market.minEnergyPrice
    printf "> "

    market

let printPlayer (market : Domain.EnergyMarket) =
    printfn "PLAYER:\nMoney to Spend: %A" market.player.moneytoSpend
    printfn "Storage Capacity: %A" market.player.storageCapacity
    printfn "Stored Energy: %A" market.player.storedEnergy
    printf "> "

    market



let read (input : string,market : Domain.EnergyMarket) =
    
    match input with
    | Sell v -> 
       let buy = Domain.PlayerAction.Buy (Domain.EnergyQuantity.OfInt v)
       Domain.processPlayerIn market buy
    | Buy v -> 
       let sell = Domain.PlayerAction.Sell (Domain.EnergyQuantity.OfInt v)
       Domain.processPlayerIn market sell
    |_->market

let evaluate (market : Domain.EnergyMarket)=
   let input=Console.ReadLine()
   read (input,market)


   

let rec loop (market : Domain.EnergyMarket) =
    Domain.processConsumersIn market
    |>printMarket 
    |>Domain.proceesProsumersIn  
    |>printMarket 
    |>printPlayer 
    |> evaluate
    |>loop
   


//repl hält liste von prosumer+spieler
//domain.processProducer
//randomized prosumer+spieler
//print energy and price
//bei prosumer -> domain.processProsumer id market -> market
//print energy and price
//bei spieler -> print money and energy 
//read spieler desicion -> domain.processPlayer desicion market -> market
//process restliche prosumer -> domain.processProsumer id
//domain.processConsumer
