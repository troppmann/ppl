module Problems
  ( indiaGpaProblem,
    add2Uniform,
  )
where
import Representation

indiaGpaProblem = "if Uniform < 0.5 then (0, if Uniform < 0.01 then 4 else Uniform * 4) else (1, if Uniform < 0.01 then 10 else Uniform * 10)"
add2Uniform = "if Uniform < 0.5 then Uniform ** 0.5 else 2 - ((1 - Uniform) ** 0.5)"
geometric = [("main", IfThenElse (LessThan Uniform (Const $ VFloat 0.2)) (Const $ VFloat 1.0) (Plus (Const $ VFloat 1.0) (FnCall "main" [])))]
