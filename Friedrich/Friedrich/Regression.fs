namespace Friedrich

module Regression =

    open System
    open MathNet.Numerics.LinearAlgebra.Double

    // 0 out first element of Theta for regularization.
    let private reducedTheta (theta: Vector) = 
        theta 
        |> Vector.mapi (fun i x -> if i = 0 then 0. else x)

    // cost function: sum of squares of distance
    // between observed and predicted, and 
    // regularization term (norm of Theta)
    let cost (x: DenseMatrix) (y: DenseVector) (w: DenseVector) (theta: DenseVector) (lambda: float) = 
        let m = y.Count |> (float)
        let theta' = reducedTheta theta
        ((y - x * theta).PointwiseMultiply(w) * (y - x * theta)) / (1. * m) 
        + (lambda / (2. * m)) * (theta' * theta')

    let private descentUpdate (x: DenseMatrix) (y: DenseVector) (w: DenseVector) (theta: DenseVector) (alpha: float) (lambda: float) =
        let m = y.Count |> (float)
        let gradient = 
            ((1. / m) * (y - (x * theta)).PointwiseMultiply(w) * x
            - (1. / m) * lambda * (reducedTheta theta)) :?> DenseVector
        (theta + alpha * gradient) 

    let private run (x: DenseMatrix) (y: DenseVector) (w: DenseVector) (intercept:bool) (alpha: float) (lambda: float) (iters: int) =
        if not (x.RowCount = y.Count) 
        then failwith "Inconsistent sizes"
        else
            let features = x.ColumnCount
            let x' =
                if intercept 
                then x.InsertColumn(0, DenseVector.create y.Count 1.) :?> DenseMatrix
                else  x.InsertColumn(0, DenseVector.create y.Count 0.) :?> DenseMatrix
            let theta = DenseVector(features + 1)

            let rec updater i t =

                let t' = descentUpdate x' y w t alpha lambda
                let curr = cost x' y w t' lambda
                System.Console.WriteLine(curr)
                if i > iters then t'
                else updater (i + 1) t'

            updater 0 theta 

    let linear (x: DenseVector) (th: DenseVector) = (Vector.insert 0 1. x * th)
  
    let estimate (data: float [] * float [][] * float []) // output, inputs, obs weights
                 intercept alpha lambda iters =

        let Ys, Xs, Ws = data

        let features = Xs.[0].Length
        let sample = Ys.Length
    
        // convert to algebra form
        let y = DenseVector(sample)
        let x = DenseMatrix(sample, features)
        let w = DenseVector(sample)
    
        // fill in matrices & vectors
        for row in 0 .. (sample - 1) do
            for col in 0 .. (features - 1) do
                x.[row,col] <- Xs.[row].[col]
            y.[row] <- Ys.[row]
            w.[row] <- Ws.[row]

        run x y w intercept alpha lambda iters    