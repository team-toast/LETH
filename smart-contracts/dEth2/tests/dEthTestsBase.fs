module dEthTestsBase

open TestBase
open Nethereum.Util
open System.Numerics
open SolidityTypes

module Array = 
    let removeFromEnd elem = Array.rev >> Array.skipWhile (fun i -> i = elem) >> Array.rev

let protocolFeePercent = bigint 9 * BigInteger.Pow(bigint 10, 15)
let hundredPerc = BigInteger.Pow(bigint 10, 18)
let onePerc = BigInteger.Pow(bigint 10, 16)
let ratio = BigInteger.Pow(bigint 10, 34)

let RAY = BigInteger.Pow(bigint 10, 27);
let rdiv x y = (x * RAY + y / bigint 2) / y;

let WAD = BigInteger.Pow(bigint 10, 18);
let wmul x y = ((x * y) + WAD / bigint 2) / WAD 

let dEthMainnetOwner = "0xb7c6bb064620270f8c1daa7502bcca75fc074cf4"
let dEthMainnet = "0x5420dFecFaCcDAE68b406ce96079d37743Aa11Ae"

let gulper = "0xa3cC915E9f1f81185c8C6efb00f16F100e7F07CA"
let proxyCache = "0x271293c67E2D3140a0E9381EfF1F9b01E07B0795"
let cdpId = bigint 18963
let makerManager = "0x5ef30b9986345249bc32d8928B7ee64DE9435E39"
let ethGemJoin = "0x2F0b23f53734252Bda2277357e97e1517d6B042A"
let saverProxy = "0xC563aCE6FACD385cB1F34fA723f412Cc64E63D47"
let saverProxyActions = "0x82ecD135Dce65Fbc6DbdD0e4237E0AF93FFD5038"
let initialRecipient = "0xb7c6bb064620270f8c1daa7502bcca75fc074cf4"
[<Literal>]
let foundryTreasury = "0x93fE7D1d24bE7CB33329800ba2166f4D28Eaa553"
let dsGuardFactory = "0x5a15566417e6C1c9546523066500bDDBc53F88C7"
let cdpOwner = "0xBA1a28b8c69Bdb92d0c898A0938cd2814dc2cA5A"
let cat = "0xa5679c04fc3d9d8b0aab1f0ab83555b301ca70ea"
let vat = "0x35d1b3f3d7966a1dfe207aa4514c12a259a0492b"
let spot = "0x65C79fcB50Ca1594B025960e539eD7A9a6D434A3"
let ilk = "ETH-A"
let ilkPIPAuthority = "0xBE8E3e3618f7474F8cB1d074A26afFef007E98FB"
let ilkFlipper = "0xF32836B9E1f47a0515c6Ec431592D5EbC276407f"
let ilkFlipperAuthority = "0xBE8E3e3618f7474F8cB1d074A26afFef007E98FB"
let daiMainnet = "0x6b175474e89094c44da98b954eedeac495271d0f"
[<Literal>]
let repaymentRatio = 180
[<Literal>]
let targetRatio = 220
[<Literal>]
let boostRatio = 220

let makerOracle = Contracts.MakerOracleMockContract(ethConn.GetWeb3)
let daiUsdOracle = Contracts.ChainLinkPriceOracleMockContract(ethConn.GetWeb3)
let ethUsdOracle = Contracts.ChainLinkPriceOracleMockContract(ethConn.GetWeb3)

let makerOracleMainnet = "0x729D19f657BD0614b4985Cf1D82531c67569197B"
let daiUsdMainnet = "0xAed0c38402a5d19df6E4c03F4E2DceD6e29c1ee9"
let ethUsdMainnet = "0x5f4eC3Df9cbd43714FE2740f5E3616155c5b8419"
let oracleContract = Contracts.OracleContract(ethConn.GetWeb3, makerOracle.Address, daiUsdOracle.Address, ethUsdOracle.Address)
let oracleContractMainnet = Contracts.OracleContract(ethConn.GetWeb3, makerOracleMainnet, daiUsdMainnet, ethUsdMainnet)

let vatContract = Contracts.VatLikeContract(vat, ethConn.GetWeb3)
let makerManagerAdvanced = Contracts.IMakerManagerAdvancedContract(makerManager, ethConn.GetWeb3)
let getGulperEthBalance () = gulper |> ethConn.GetEtherBalance

let toMakerPriceFormatDecimal (a:decimal) = (BigDecimal(a) * (BigDecimal.Pow(10.0, 18.0))).Mantissa
let toMakerPriceFormat = decimal >> toMakerPriceFormatDecimal

let toChainLinkPriceFormatDecimal (a:decimal) = (BigDecimal(a) * (BigDecimal.Pow(10.0, 8.0))).Mantissa
let toChainLinkPriceFormatInt (a:int) = toChainLinkPriceFormatDecimal <| decimal a

let initOracles priceMaker priceDaiUsd priceEthUsd =
    makerOracle.setData(toMakerPriceFormat priceMaker) |> ignore
    daiUsdOracle.setData(toChainLinkPriceFormatDecimal priceDaiUsd) |> ignore
    ethUsdOracle.setData(toChainLinkPriceFormatDecimal priceEthUsd) |> ignore
    
// percent is normalized to range [0, 1]
let initOraclesDefault percentDiffNormalized =
    let priceMaker = 10 // can be any value
    let priceDaiUsd = 5 // can be any value
    let priceNonMakerDaiEth = (decimal priceMaker + (decimal priceMaker) * percentDiffNormalized)
    let priceEthUsd = priceNonMakerDaiEth / decimal priceDaiUsd
    
    initOracles (decimal priceMaker) (decimal priceDaiUsd) priceEthUsd

    decimal priceMaker, decimal priceDaiUsd, priceNonMakerDaiEth, priceEthUsd

let getDEthContractFromOracle (oracleContract:Contracts.OracleContract) initialRecipientIsTestAccount =
    let initialRecipient = if initialRecipientIsTestAccount then ethConn.Account.Address else initialRecipient

    let contract = Contracts.dEthContract(ethConn.GetWeb3, gulper, cdpId, oracleContract.Address, initialRecipient, foundryTreasury)

    let authorityAddress = contract.authorityQuery()
    let authority = Contracts.DSAuthorityContract(authorityAddress, ethConn.GetWeb3)

    authority, contract

let getDEthContractAndAuthority () =
    getDEthContractFromOracle oracleContractMainnet false

let getDEthContract () = 
    let _, contract = getDEthContractAndAuthority ()
    contract

let getDEthContractEthConn () =
    let _, contract = getDEthContractFromOracle oracleContractMainnet true
    
    ethConn.MakeImpersonatedCallWithNoEther dEthMainnet makerManager (Contracts.ManagerLikeContract.giveFunction(Prop0 = cdpId, Prop1 = contract.Address)) 
    |> shouldSucceed

    // check that we now own the cdp.
    let makerManagerContract = Contracts.IMakerManagerAdvancedContract(makerManager, ethConn.GetWeb3)
    let cdpOwner = makerManagerContract.ownsQuery(cdpId)
    cdpOwner |> shouldEqualIgnoringCase contract.Address

    contract

let dEthContract = getDEthContractEthConn ()

let getMockDSValueFormat (priceFormatted:BigInteger) =
    let dSValueMock = Contracts.DSValueMockContract(ethConn.GetWeb3)
    dSValueMock.setData(priceFormatted) |> ignore

    dSValueMock

let getMockDSValue price = toMakerPriceFormat price |> getMockDSValueFormat

let getManuallyComputedCollateralValues (oracleContract: Contracts.OracleContract) saverProxy (cdpId:bigint) =
    let priceEthDai = oracleContract.getEthDaiPriceQuery()
    let priceRay = BigInteger.Multiply(BigInteger.Pow(bigint 10, 9), priceEthDai)
    let saverProxy = Contracts.MCDSaverProxyContract(saverProxy, ethConn.GetWeb3)
    let cdpDetailedInfoOutput = saverProxy.getCdpDetailedInfoQuery(cdpId)
    let collateralDenominatedDebt = rdiv cdpDetailedInfoOutput.debt priceRay
    let excessCollateral = cdpDetailedInfoOutput.collateral - collateralDenominatedDebt

    (priceEthDai, priceRay, saverProxy, cdpDetailedInfoOutput, collateralDenominatedDebt, excessCollateral)

let getInkAndUrnFromCdp (cdpManagerContract:Contracts.IMakerManagerAdvancedContract) cdpId =
    let ilkBytes = cdpManagerContract.ilksQuery(cdpId) |> Array.removeFromEnd (byte 0)
    let urn = cdpManagerContract.urnsQuery(cdpId)
    (ilkBytes, urn)

let getInk () =
    let (ilk, urn) = getInkAndUrnFromCdp makerManagerAdvanced cdpId
    
    (vatContract.urnsQuery(ilk, urn)).Prop0

let findActiveCDP ilkArg =
    let cdpManagerContract = Contracts.IMakerManagerAdvancedContract(makerManager, ethConn.GetWeb3)
    let vatContract = Contracts.VatLikeContract(vat, ethConn.GetWeb3)
    let maxCdpId = cdpManagerContract.cdpiQuery()
    
    let cdpIds = (Seq.initInfinite (fun i -> maxCdpId - bigint i) ) |> Seq.takeWhile (fun i -> i > BigInteger.Zero)

    let getInkAndUrnFromCdp = getInkAndUrnFromCdp cdpManagerContract
    let isCDPActive cdpId =
        let (ilkBytes, urn) = getInkAndUrnFromCdp cdpId
        let ilk = System.Text.Encoding.UTF8.GetString(ilkBytes)
        let urnsOutput = vatContract.urnsQuery(ilkBytes,urn)

        urnsOutput.Prop1 <> bigint 0 && urnsOutput.Prop0 <> bigint 0 && ilk = ilkArg

    let cdpId = Seq.findBack isCDPActive cdpIds

    getInkAndUrnFromCdp cdpId

let pokePIP pipAddress = 
    ethConn.TimeTravel <| Constants.hours * 2UL
    
    ethConn.MakeImpersonatedCallWithNoEther ilkPIPAuthority pipAddress ( Contracts.IMakerOracleAdvancedContract.pokeFunction()) |> ignore

let calculateRedemptionValue tokensToRedeem totalSupply excessCollateral automationFeePerc =
    let redeemTokenSupplyPerc = tokensToRedeem * hundredPerc / totalSupply
    let collateralAffected = excessCollateral * redeemTokenSupplyPerc / hundredPerc
    let protocolFee = collateralAffected * protocolFeePercent / hundredPerc
    let automationFee = collateralAffected * automationFeePerc / hundredPerc;
    let collateralRedeemed = collateralAffected - automationFee; // how much capital should exit the dEth contract
    let collateralReturned = collateralAffected - protocolFee - automationFee; // how much capital should return to the user

    (protocolFee, automationFee, collateralRedeemed, collateralReturned)

let queryStateAndCalculateRedemptionValue (dEthContract:Contracts.dEthContract) tokensAmount =
    calculateRedemptionValue tokensAmount (dEthContract.totalSupplyQuery()) (dEthContract.getExcessCollateralQuery()) (dEthContract.automationFeePercQuery())

let calculateIssuanceAmount suppliedCollateral automationFeePerc excessCollateral totalSupply =
    let protocolFee = suppliedCollateral * protocolFeePercent / hundredPerc
    let automationFee = suppliedCollateral * automationFeePerc / hundredPerc
    let actualCollateralAdded = suppliedCollateral - protocolFee; // protocolFee goes to the protocol 
    let accreditedCollateral = actualCollateralAdded - automationFee; // automationFee goes to the pool of funds in the cdp to offset gas implications
    let newTokenSupplyPerc = accreditedCollateral * hundredPerc / excessCollateral
    let tokensIssued = totalSupply * newTokenSupplyPerc / hundredPerc
    
    (protocolFee, automationFee, actualCollateralAdded, accreditedCollateral, tokensIssued)

let queryStateAndCalculateIssuanceAmount (dEthContract:Contracts.dEthContract) weiValue = 
    calculateIssuanceAmount weiValue (dEthContract.automationFeePercQuery()) (dEthContract.getExcessCollateralQuery()) (dEthContract.totalSupplyQuery())

let makeRiskLimitLessThanExcessCollateral (dEthContract:Contracts.dEthContract) =
    let excessCollateral = dEthContract.getExcessCollateralQuery()
    let ratioBetweenRiskLimitAndExcessCollateral = 0.9M // hardcoded to be less than one - so that risk limit is less than excess collateral
    let riskLimit = toBigDecimal excessCollateral * BigDecimal(ratioBetweenRiskLimitAndExcessCollateral) |> toBigInt
    dEthContract.changeSettings((dEthContract.minRedemptionRatioQuery()) / ratio, dEthContract.automationFeePercQuery(), riskLimit)

// note: this is used to be able to specify owner and contract addresses in inlinedata (we cannot use DUs in attributes)
let mapInlineDataArgumentToAddress inlineDataArgument calledContractAddress =
    match inlineDataArgument with
      | "owner" -> ethConn.Account.Address // we assume that the called contract is "owned" by our connection
      | "contract" -> calledContractAddress
      | _ -> inlineDataArgument

// this is a mechanism of being able to revert to the same snapshot over and over again.
// when we call restore, the snapshot we restore to gets deleted. So we need to create a new one immediatelly after that.
// this is put in this module because we need to get snapshot at the point when every static state in this module is initialized
let mutable snapshotId = ethConn.MakeSnapshot()
let restore () =
    ethConn.RestoreSnapshot snapshotId
    snapshotId <- ethConn.MakeSnapshot ()