type EnergyPrice = decimal

type EnergyQuantity = 
    private
    | EnergyQuantity of int
    static member OfInt (quant : int) =
            if quant < 0 then
            failwith "That won't do, friend."
            else
            EnergyQuantity quant
    static member (+) ((EnergyQuantity a), (EnergyQuantity b)) =
        if (a + b) < 0 then
            EnergyQuantity 0
        else
            EnergyQuantity (a + b)
    static member (-) ((EnergyQuantity a), (EnergyQuantity b)) =
        if (a - b) < 0 then
            EnergyQuantity 0
        else
            EnergyQuantity (a - b)

type EnergyProducer = 
    {producedEnergy : EnergyQuantity
     }


type EnergyConsumer = 
    {consumedEnergy : EnergyQuantity
     }

type EnergyProsumer = 
    {
        storedEnergy : EnergyQuantity
        storageCapacity :EnergyQuantity
        buyingPrice : EnergyPrice
        sellingPrice :EnergyPrice
        isPlayer : bool
    }



type MarketParticipant =
    |Generator of EnergyProducer
    |Prosumer of EnergyProsumer
    |Load of EnergyConsumer
    


type EnergyMarket = 
    {
        marketProducers : EnergyProducer list
        marketProsumers : EnergyProsumer list
        marketConsumers : EnergyConsumer list 
        excessEnergy : EnergyQuantity
        storageCapacity :EnergyQuantity
    }

let addEnergyProducerToMarket (gen : EnergyProducer) (market:EnergyMarket) : EnergyMarket =
    market.marketProducers = gen::market.marketProducers
    market

let addParticipantToMarket (participant: MarketParticipant) (market:EnergyMarket) =
    match participant with
    |Generator g -> ()
    |Prosumer p -> ()
    |Load l -> ()




let a : EnergyQuantity = EnergyQuantity.OfInt 1

let b : EnergyQuantity = EnergyQuantity.OfInt 2


a+b
