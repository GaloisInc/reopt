-- A specialised pretty printer for FnRep with recovered type information
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Pretty (ppFunction) where

import qualified Data.ByteString.Char8             as BSC
import           Data.Macaw.CFG
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Parameterized                (ShowF, Some (Some),
                                                    viewSome)
import qualified Data.Vector                       as V
import           Prettyprinter
import           Reopt.CFG.FnRep
import           Reopt.TypeInference.ConstraintGen (FunType (..),
                                                    ITy, FTy,
                                                    ModuleConstraints (..))
import           Reopt.TypeInference.Constraints   (Ty (UnknownTy))

-- | Utility to pretty print with commas separating arguments.
commas :: [Doc a] -> Doc a
commas = hsep . punctuate comma

ppFnAssignId :: Map FnAssignId FTy -> FnAssignId -> Doc ()
ppFnAssignId tyvs aid
  | Just ty <- Map.lookup aid tyvs =
               "(" <> pretty aid <+> "::" <+> pretty ty <> ")"
  | otherwise = pretty aid

ppAssignment :: FnArchConstraints arch =>
                Map FnAssignId FTy -> FnAssignment arch ty -> Doc ()
ppAssignment tyvs (FnAssignment lhs rhs) =
  ppFnAssignId tyvs lhs <> " := " <> pretty rhs

ppFnStmt ::  ( FnArchConstraints arch
           , IsArchStmt (FnArchStmt arch)
           ) => Map FnAssignId FTy -> FnStmt arch -> Doc ()
ppFnStmt tyvs (FnAssignStmt assign) = ppAssignment tyvs assign
ppFnStmt tyvs (FnCall f args (Just (Some r))) =
  let argDocs = (\(Some v) -> pretty v) <$> args
  in ppFnAssignId tyvs (frAssignId r) <> " := call" <+> pretty f <> parens (commas argDocs)
ppFnStmt _ s = pretty s

ppBlock :: (FnArchConstraints arch
           , ShowF (ArchReg arch)
           , IsArchStmt (FnArchStmt arch)
           ) => Map FnAssignId FTy -> FnBlock arch -> Doc ()
ppBlock tyvs b =
  pretty (fbLabel b) <+> encloseSep lbracket rbracket " " phiVars <> hardline
  <> indent 2 (phiBindings <> stmts <> tstmt)
  where
    ppGenType bty aid
      | Just ty <- Map.lookup aid tyvs = "<<" <> pretty ty <> ">>"
      | otherwise = pretty bty

    ppPhiName v = parens (pretty (unFnPhiVar v) <+> ppGenType (fnPhiVarType v) (unFnPhiVar v))
    phiVars = V.toList $ viewSome ppPhiName <$> fbPhiVars b
    ppBinding v l = parens ("mc_binding " <> v <+> pretty l) <> hardline

    ppPhiBindings :: ShowF (ArchReg arch) => Some (FnPhiVar arch) -> Doc a
    ppPhiBindings (Some v) = foldMap (ppBinding (pretty (unFnPhiVar v))) locs
      where locs = fnPhiVarRep v : fnPhiVarLocations v

    phiBindings = foldMap ppPhiBindings (fbPhiVars b)
    stmts = foldMap (\s -> ppFnStmt tyvs s <> hardline) (fbStmts b)
    tstmt = pretty (fbTerm b)

ppFunction :: (FnArchConstraints arch
              , ShowF (ArchReg arch)
              , IsArchStmt (FnArchStmt arch)
              ) => ModuleConstraints arch -> Function arch -> Doc ()
ppFunction mcs fn
  | Just tyvs <- Map.lookup (fnName fn) (mcAssignTyVars mcs)
  , Just fty  <- Map.lookup (fnAddr fn) (mcFunTypes mcs)
  = let tyvs' = Map.compose (mcTypeMap mcs) tyvs
        atp = parens (commas (zipWith ppArg [0..] (funArgs fty)))
        rtp = maybe "void" ppTy (funRet fty)
    in vcat [ "function " <> nm <> " @ " <> addr <> atp <> " : " <> rtp
            , lbrace
            , nest 4 $ vcat (ppBlock tyvs' <$> fnBlocks fn)
            , rbrace
            ]
  where
    nm = pretty (BSC.unpack (fnName fn))
    addr = pretty (fnAddr fn)


    -- non
    ppTy (UnknownTy tyv)
      | Just tp <- Map.lookup tyv (mcTypeMap mcs) = pretty tp
      | otherwise = "???"
    ppTy ty = pretty ty

    ppArg :: Integer -> ITy -> Doc a
    ppArg i tp = "arg" <> pretty i <> " : " <> ppTy tp

-- Unknown type/unknown assigns  
ppFunction _mcs fn = pretty fn

