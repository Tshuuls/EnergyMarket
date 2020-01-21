module Repl

open System
open Parser

open Microsoft.FSharp.Reflection

let printMarket (market : Domain.EnergyMarket) =
    printfn "MARKET:\nStorage Capacity: %A" market.storageCapacity
    printfn "Exess Energy: %A" market.excessEnergy
    printfn "Energy Price: %A\n" market.energyPrice
    market

let printPlayer (market : Domain.EnergyMarket) =
    printfn "PLAYER:\nMoney to Spend: %A" market.player.moneytoSpend
    printfn "Storage Capacity: %A" market.player.storageCapacity
    printfn "Stored Energy: %A\n" market.player.storedEnergy
    printf "> "
    market

let read (input : string,market : Domain.EnergyMarket) =
    match input with
    | Sell v -> 
       let sell = Domain.PlayerAction.Sell (Domain.EnergyQuantity.OfInt v)
       Domain.processPlayerIn market sell
    | Buy v -> 
       let buy = Domain.PlayerAction.Buy (Domain.EnergyQuantity.OfInt v)
       Domain.processPlayerIn market buy
    |_->market

let evaluate (market : Domain.EnergyMarket)=
   let input=Console.ReadLine()
   read (input,market)

let rec loop (market : Domain.EnergyMarket) =
    printfn "\n\nNew Day new Luck\n"
    Domain.processProducerssIn market
    |>printMarket 
    |>printPlayer 
    |> evaluate
    |>Domain.proceesProsumersIn  
    |>printMarket
    |> Domain.processConsumersIn  
    |>printMarket
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
