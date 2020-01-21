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
   
let init ()=
    printfn "\n\nConfigure Producer\nEnter Produced Energy"
    let producer = Domain.Generator ({producedEnergy = Domain.EnergyQuantity.OfInt (Console.ReadLine() |> int )})
    
    printfn "\n\nConfigure Prosumer\nEnter stored Energy"
    let storedEnergy=Domain.EnergyQuantity.OfInt(Console.ReadLine() |> int )
    printfn "Enter storage Capacity"
    let storageCapacity=Domain.EnergyQuantity.OfInt(Console.ReadLine() |> int )
    printfn "Enter buying Price"
    let buyingPrice=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )
    printfn "Enter selling Price"
    let sellingPrice=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )
    printfn "Enter money to Spend"
    let moneytoSpend=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )

    let pros1 :Domain.EnergyProsumer={
       id =1
       storedEnergy = storedEnergy
       storageCapacity = storageCapacity
       buyingPrice = buyingPrice
       sellingPrice =sellingPrice
       moneytoSpend = moneytoSpend
    }

    printfn "\n\nConfigure Prosumer\nEnter stored Energy"
    let storedEnergy=Domain.EnergyQuantity.OfInt(Console.ReadLine() |> int )
    printfn "Enter storage Capacity"
    let storageCapacity=Domain.EnergyQuantity.OfInt(Console.ReadLine() |> int )
    printfn "Enter buying Price"
    let buyingPrice=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )
    printfn "Enter selling Price"
    let sellingPrice=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )
    printfn "Enter money to Spend"
    let moneytoSpend=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )

    let pros2 :Domain.EnergyProsumer={
       id =2
       storedEnergy = storedEnergy
       storageCapacity = storageCapacity
       buyingPrice = buyingPrice
       sellingPrice =sellingPrice
       moneytoSpend = moneytoSpend
    }

    printfn "\n\nConfigure Consumer\nEnter consumed Energy"
    let consumer =Domain.Load {consumedEnergy = Domain.EnergyQuantity.OfInt (Console.ReadLine() |> int )} 

    printfn "\n\nConfigure Player\nEnter stored Energy"
    let storedEnergy=Domain.EnergyQuantity.OfInt(Console.ReadLine() |> int )
    printfn "Enter storage Capacity"
    let storageCapacity=Domain.EnergyQuantity.OfInt(Console.ReadLine() |> int )
    printfn "Enter Money to spend"
    let moneytoSpend=Domain.Money.OfDecimal(Console.ReadLine() |> decimal )

    let player1 :Domain.Player= {
        storedEnergy = storedEnergy
        storageCapacity =storageCapacity
        moneytoSpend = moneytoSpend
    }

    let marketA :Domain.EnergyMarket ={
     marketProducers = []
     marketProsumers = []
     marketConsumers = []
     excessEnergy = Domain.EnergyQuantity.OfInt 25
     storageCapacity =Domain.EnergyQuantity.OfInt 500
     energyPrice = Domain.Money.OfDecimal (501M - 25M)
     maxEnergyPrice = Domain.Money.OfDecimal 500M
     minEnergyPrice = Domain.Money.OfDecimal 1M
     player =player1
    }

    Domain.addParticipantToMarket (producer) marketA
    |>Domain.addParticipantToMarket (consumer) 
    |>Domain.addParticipantToMarket (Domain.Prosumer pros1) 
    |>Domain.addParticipantToMarket (Domain.Prosumer pros2) 


(*let pro2 :EnergyProsumer = {
      id =2
      storedEnergy = EnergyQuantity.OfInt 300
      storageCapacity = EnergyQuantity.OfInt 350
      buyingPrice = Money.OfDecimal 100M
      sellingPrice =Money.OfDecimal 200M
      moneytoSpend = Money.OfDecimal 200M
      roundplayerd = false
    }*)


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
