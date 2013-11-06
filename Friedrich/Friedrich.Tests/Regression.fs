namespace Friedrich.Tests

open MathNet
open Friedrich.Regression
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Regression unit tests`` () = 
    
    [<Test>]
    member this.``Simple Regression, intercept`` () =

        // Y = 1 + 2 * X1 - 3 * X2

        let X = 
            [|  [| 1.; 1. |];
                [| 1.; 0. |];
                [| 0.; 1. |];
            |]

        let Y = [| 0.; 3.; -2. |]
        let W = [| 1.; 1.; 1.; |]

        let result = estimate (Y,X,W) true 0.5 0. 100
        
        result.[0] |> should (equalWithin 0.1) 1.
        result.[1] |> should (equalWithin 0.1) 2.
        result.[2] |> should (equalWithin 0.1) -3.

    [<Test>]
    member this.``Simple Regression, no intercept`` () =

        // Y = 0 + 2 * X1

        let X = 
            [|  [| 1.; |];
                [| 1.; |];
            |]

        let Y = [| 1.; 3.; |] // off the diagonal by +1, -1
        let W = [| 1.; 1.; |]

        let result = estimate (Y,X,W) false 0.5 0. 10
        
        result.[0] |> should (equalWithin 0.1) 0.
        result.[1] |> should (equalWithin 0.1) 2.

    [<Test>]
    member this.``Regularized Regression, intercept`` () =

        // Y = 1 + 2 * X1 - 3 * X2

        let X = 
            [|  [| 1.; 1. |];
                [| 1.; 0. |];
                [| 0.; 1. |];
            |]

        let Y = [| 0.; 3.; -2. |]
        let W = [| 1.; 1.; 1.; |]

        let result = estimate (Y,X,W) true 0.5 0.1 100
        
        // Computed using another solver
        result.[0] |> should (equalWithin 0.1) 0.84
        result.[1] |> should (equalWithin 0.1) 1.89
        result.[2] |> should (equalWithin 0.1) -2.66