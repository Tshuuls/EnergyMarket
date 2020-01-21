module Domain

type EnergyQuantity = 
    private
    | EnergyQuantity of int
    static member OfInt (quant : int) =
            if quant < 0 then
                failwith "That won't do, friend."
            else
                EnergyQuantity quant
    static member ToInt (EnergyQuantity a) =
            int a
    static member (+) ((EnergyQuantity a), (EnergyQuantity b)) =
            EnergyQuantity (a + b)
    static member (-) ((EnergyQuantity a), (EnergyQuantity b)) =
        if (a - b) < 0 then
            EnergyQuantity 0
        else
            EnergyQuantity (a - b)
    static member (*) ((a:decimal), (EnergyQuantity b)) =
            EnergyQuantity ((int)a * b)

type Money = 
    private
    | Money of decimal
    static member OfDecimal (quant : decimal) =
            if quant < 0M then
                failwith "That won't do, friend."
            else
                Money quant
    static member OfInt (quant : int) =
            if quant < 0 then
                failwith "That won't do, friend."
            else
                Money (decimal quant)
    static member ToDecimal (Money a) =
            decimal a
    static member (+) ((Money a), (Money b)) =
            Money (a + b)
    static member (-) ((Money a), (Money b)) =
        if (a - b) < 0M then
            Money 0M
        else
            Money (a - b)
    static member (/) ((Money a), (Money b)) =
            decimal (a / b)
    static member (*) ((Money a), ( b:decimal)) =
            Money (a * b)

let MoneyTimesQuantity (a:Money ) (b:EnergyQuantity) =
    a * (decimal (EnergyQuantity.ToInt b))


type EnergyProducer = 
    {producedEnergy : EnergyQuantity
     }


type EnergyConsumer = 
    {consumedEnergy : EnergyQuantity
     }

type EnergyProsumer = 
    {
        id : int
        storedEnergy : EnergyQuantity
        storageCapacity :EnergyQuantity
        buyingPrice : Money
        sellingPrice :Money
        moneytoSpend : Money
        roundplayerd: bool
    }

type Player = 
    {
        storedEnergy : EnergyQuantity
        storageCapacity :EnergyQuantity
        moneytoSpend : Money
    }

type PlayerAction = 
    |Buy of EnergyQuantity
    |Sell of EnergyQuantity


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
        energyPrice : Money
        maxEnergyPrice : Money
        minEnergyPrice : Money
        player : Player
    }

let addEnergyProducerToMarket (gen : EnergyProducer) (market:EnergyMarket) : EnergyMarket =
    {market with marketProducers = gen::market.marketProducers 
    }
 
let addEnergyProsumerToMarket (pro : EnergyProsumer) (market:EnergyMarket) : EnergyMarket =
     {market with  marketProsumers = pro::market.marketProsumers
    }

let addEnergyConsumerToMarket (load : EnergyConsumer) (market:EnergyMarket) : EnergyMarket =
   {market with marketConsumers = load::market.marketConsumers
    }

let addParticipantToMarket (participant: MarketParticipant) (market:EnergyMarket) : EnergyMarket = 
    match participant with
    |Generator g -> addEnergyProducerToMarket g market
    |Prosumer p -> addEnergyProsumerToMarket p market
    |Load l -> addEnergyConsumerToMarket l market

let calculatePrice (capacity: EnergyQuantity) (excessEnergy: EnergyQuantity) : Money = 
    Money.OfInt (EnergyQuantity.ToInt (capacity - excessEnergy));

let updateEnergyPrice (market:EnergyMarket)  =
    { market with energyPrice = calculatePrice market.storageCapacity market.excessEnergy}

let proceesProducerForMarket (gen : EnergyProducer) (market:EnergyMarket)  =
    updateEnergyPrice { market with excessEnergy = market.excessEnergy + gen.producedEnergy}

let processProducerssIn (market:EnergyMarket) : EnergyMarket =
    List.foldBack proceesProducerForMarket market.marketProducers market
    
let replaceProsumerIn (pro : EnergyProsumer) (market:EnergyMarket) =
    let prosumersNew = market.marketProsumers 
                       |> List.map (fun proList -> if proList.id = pro.id then pro else proList)
    {market with marketProsumers = prosumersNew}
 
let prosumerBuyesFromMarket (pro : EnergyProsumer) (market:EnergyMarket)  =
    let amountPossibleMarket =  market.excessEnergy
    let amountPossibleProsumerStorage = pro.storageCapacity-pro.storedEnergy
    let amountPossible =
        if(amountPossibleMarket<amountPossibleProsumerStorage) then
            amountPossibleMarket
        else
           amountPossibleProsumerStorage
    let priceForAll = MoneyTimesQuantity market.energyPrice amountPossible
    let price = 
        if(priceForAll> pro.moneytoSpend) then
            pro.moneytoSpend
        else
            priceForAll
    let amountBought = (price / priceForAll) * amountPossible
    let prosumerNew = {pro with storedEnergy = pro.storedEnergy + amountBought;moneytoSpend = pro.moneytoSpend-price}
    let marketNew = {market with excessEnergy = market.excessEnergy - amountBought} 
    replaceProsumerIn prosumerNew marketNew
   
let prosumerSellsToMarket (pro : EnergyProsumer) (market:EnergyMarket)  =
    let amountPossibleMarket = market.storageCapacity - market.excessEnergy
    let amountPossibleProsumerStorage = pro.storedEnergy
    let amountPossible =
        if(amountPossibleMarket<amountPossibleProsumerStorage) then
            amountPossibleMarket
        else
           amountPossibleProsumerStorage
   
    let price = MoneyTimesQuantity market.energyPrice amountPossible
    let amountBought = amountPossible
    let prosumerNew = {pro with storedEnergy = pro.storedEnergy - amountBought;moneytoSpend =  pro.moneytoSpend+price}
    let marketNew =  {market with excessEnergy = market.excessEnergy + amountBought} 
    replaceProsumerIn prosumerNew marketNew

let proceesProsumerIndexForMarket (index : int) (market:EnergyMarket)  =
    let pro = List.item index market.marketProsumers 
    if(market.energyPrice < pro.buyingPrice && pro.storageCapacity>pro.storedEnergy && market.excessEnergy>EnergyQuantity.OfInt 0) then
                        prosumerBuyesFromMarket pro market
                     elif(market.energyPrice > pro.sellingPrice && pro.storedEnergy>EnergyQuantity.OfInt 0) then
                        prosumerSellsToMarket pro market
                        else market
    |> updateEnergyPrice 

let proceesProsumerForMarket (pro : EnergyProsumer) (market:EnergyMarket)  =
    if(market.energyPrice < pro.buyingPrice && pro.storageCapacity>pro.storedEnergy) then
                        prosumerBuyesFromMarket pro market
                     elif(market.energyPrice > pro.sellingPrice && pro.storedEnergy>EnergyQuantity.OfInt 0) then
                        prosumerSellsToMarket pro market
                        else market
    |> updateEnergyPrice 

let proceesProsumersIn (market:EnergyMarket) =
    List.foldBack proceesProsumerForMarket market.marketProsumers market

let proceesConsumersForMarket (load : EnergyConsumer) (market:EnergyMarket)  =
    if market.excessEnergy < load.consumedEnergy then
                failwith "Game Over!! To less Energy - Net Collapsed"
    updateEnergyPrice {market with excessEnergy = market.excessEnergy - load.consumedEnergy}

let processConsumersIn (market:EnergyMarket)  =
    List.foldBack proceesConsumersForMarket market.marketConsumers market

let playerBuysOf  (market:EnergyMarket) (amount:EnergyQuantity) =
    let price = MoneyTimesQuantity market.energyPrice  amount
    let amountPossibleStorage = market.player.storageCapacity-market.player.storedEnergy
    if(market.player.moneytoSpend >= price && amount < amountPossibleStorage) then
        let marketNew = {market with excessEnergy = market.excessEnergy-amount} |> updateEnergyPrice 
        let playerNew = {market.player with storedEnergy = market.player.storedEnergy + amount; moneytoSpend = market.player.moneytoSpend - price}
        {marketNew with player=playerNew}
    else
        market

let playerSellsTo  (market:EnergyMarket) (amount:EnergyQuantity) =
    let price = MoneyTimesQuantity market.energyPrice  amount
    let amountPossibleStorage = market.storageCapacity-market.excessEnergy
    if(amount < amountPossibleStorage && amount < market.player.storedEnergy) then
        let marketNew = {market with excessEnergy = market.excessEnergy+amount} |> updateEnergyPrice 
        let playerNew = {market.player with storedEnergy = market.player.storedEnergy - amount; moneytoSpend = market.player.moneytoSpend + price}
        {market with player=playerNew}
    else
        market
        
let processPlayerIn (market:EnergyMarket) (action:PlayerAction) =
    match action with
    | Buy x -> playerBuysOf market x
    | Sell x -> playerSellsTo market x

let init ()  =
    let player1 = {
        storedEnergy = EnergyQuantity.OfInt 8
        storageCapacity =EnergyQuantity.OfInt 5000
        moneytoSpend = Money.OfDecimal 50000M
        }
    let marketA :EnergyMarket ={
     marketProducers = []
     marketProsumers = []
     marketConsumers = []
     excessEnergy = EnergyQuantity.OfInt 25
     storageCapacity =EnergyQuantity.OfInt 500
     energyPrice = Money.OfDecimal (501M - 25M)
     maxEnergyPrice = Money.OfDecimal 500M
     minEnergyPrice = Money.OfDecimal 1M
     player =player1
    }

    let pro1 :EnergyProsumer = {
      id =1
      storedEnergy = EnergyQuantity.OfInt 200
      storageCapacity = EnergyQuantity.OfInt 400
      buyingPrice = Money.OfDecimal 200M
      sellingPrice =Money.OfDecimal 300M
      moneytoSpend = Money.OfDecimal 200M
      roundplayerd = false
    }

    let pro2 :EnergyProsumer = {
      id =2
      storedEnergy = EnergyQuantity.OfInt 300
      storageCapacity = EnergyQuantity.OfInt 350
      buyingPrice = Money.OfDecimal 100M
      sellingPrice =Money.OfDecimal 200M
      moneytoSpend = Money.OfDecimal 200M
      roundplayerd = false
    }

    addParticipantToMarket (Generator ({producedEnergy = EnergyQuantity.OfInt 6})) marketA
    |>addParticipantToMarket (Load {consumedEnergy = EnergyQuantity.OfInt 5}) 
    |>addParticipantToMarket (Prosumer pro1) 
    |>addParticipantToMarket (Prosumer pro2) 
