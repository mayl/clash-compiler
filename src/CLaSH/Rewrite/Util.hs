module CLaSH.Rewrite.Util where

import qualified Control.Monad        as Monad
import Control.Monad.Trans.Class         (lift)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State
import qualified Control.Monad.Writer as Writer
import qualified Data.HashMap.Lazy    as HashMap
import qualified Data.Label.PureM     as LabelM
import qualified Data.Monoid          as Monoid
import qualified Unbound.LocallyNameless as Unbound
import Unbound.LocallyNameless           (bind,embed,makeName,name2String,rec,unbind,unrec,unembed)

import CLaSH.Core.FreeVars (termFreeVars)
import CLaSH.Core.Pretty (showDoc)
import CLaSH.Core.Subst (substTm)
import CLaSH.Core.Term (Term(..),TmName,LetBinding)
import CLaSH.Core.Type (Type,TyName,mkTyVarTy)
import CLaSH.Core.Util (Gamma,Delta,termType,mkId,mkTyVar,mkTyLams,mkLams,mkApps,mkTyApps)
import CLaSH.Core.Var  (Var(..),Id)
import CLaSH.Rewrite.Types
import CLaSH.Util

liftR :: Monad m => m a -> RewriteMonad m a
liftR m = lift . lift . lift . lift $ m

apply :: Monad m => String -> Rewrite m -> Rewrite m
apply name rewrite ctx expr = R $ do
  (expr', anyChanged) <- Writer.listen $ runR $ rewrite ctx expr
  let hasChanged = Monoid.getAny anyChanged
  Monad.when hasChanged $ LabelM.modify transformCounter (+1)
  let (_,delta) = contextEnv ctx
  let before = showDoc delta expr
  let after  = showDoc delta expr'
  lvl <- LabelM.asks dbgLevel
  traceIf (lvl >= DebugApplied && hasChanged) ("Changes when applying rewrite " ++ name ++ " to:\n" ++ before ++ "\nResult:\n" ++ after ++ "\n") $
    traceIf (lvl >= DebugAll && not hasChanged) ("No changes when applying rewrite " ++ name ++ " to:\n" ++ before ++ "\n") $
      return expr'

runRewrite :: Monad m => String -> Rewrite m -> Term -> RewriteSession m Term
runRewrite name rewrite expr = do
  (expr',_) <- Writer.runWriterT . runR $ apply name rewrite [] expr
  return expr'

runRewriteSession ::
  Monad m
  => DebugLevel
  -> RewriteState
  -> RewriteSession m a
  -> m a
runRewriteSession lvl st
  = Unbound.runFreshMT
  . (flip State.evalStateT st)
  . (flip Reader.runReaderT (RE lvl))

setChanged :: Monad m => RewriteMonad m ()
setChanged = Writer.tell (Monoid.Any True)

changed :: Monad m => a -> RewriteMonad m a
changed val = do
  Writer.tell (Monoid.Any True)
  return val

contextEnv ::
  [CoreContext]
  -> (Gamma, Delta)
contextEnv = go HashMap.empty HashMap.empty
  where
    go gamma delta []                   = (gamma,delta)
    go gamma delta (LetBinding ids:ctx) = go gamma' delta ctx
      where
        gamma' = foldl addToGamma gamma ids

    go gamma delta (LetBody ids:ctx)    = go gamma' delta ctx
      where
        gamma' = foldl addToGamma gamma ids

    go gamma delta (LamBody lId:ctx)    = go gamma' delta ctx
      where
        gamma' = addToGamma gamma lId

    go gamma delta (TyLamBody tv:ctx)   = go gamma delta' ctx
      where
        delta' = addToDelta delta tv

    go gamma delta (CaseAlt ids:ctx)    = go gamma' delta ctx
      where
        gamma' = foldl addToGamma gamma ids

    go gamma delta (_:ctx) = go gamma delta ctx

    addToGamma gamma (Id idName ty) = HashMap.insert idName (unembed ty) gamma
    addToGamma gamma _              = error $ $(curLoc) ++ "Adding TyVar to Gamma"

    addToDelta delta (TyVar tvName ki) = HashMap.insert tvName (unembed ki) delta
    addToDelta delta _                 = error $ $(curLoc) ++ "Adding Id to Delta"

mkGamma ::
  (Functor m, Monad m)
  => [CoreContext]
  -> RewriteMonad m Gamma
mkGamma ctx = do
  let (gamma,_) = contextEnv ctx
  tsMap         <- fmap (HashMap.map fst) $ LabelM.gets bindings
  let gamma'    = tsMap `HashMap.union` gamma
  return gamma'

mkBinderFor ::
  (Functor m, Monad m)
  => [CoreContext]
  -> String
  -> Term
  -> RewriteMonad m (Id,Term)
mkBinderFor ctx name term = do
  gamma  <- mkGamma ctx
  let ty = termType gamma term
  mkInternalVar name ty

mkInternalVar ::
  (Functor m, Monad m)
  => String
  -> Type
  -> RewriteMonad m (Id,Term)
mkInternalVar name ty = do
  name' <- fmap (makeName name . toInteger) getUniqueM
  return (Id name' (embed ty),Var name')

inlineBinders ::
  Monad m
  => (LetBinding -> RewriteMonad m Bool)
  -> Rewrite m
inlineBinders condition _ expr@(Letrec b) = R $ do
  (xes,res)        <- unbind b
  (replace,others) <- partitionM condition (unrec xes)
  case replace of
    [] -> return expr
    _  -> do
      let (others',res') = substituteBinders replace others res
      let newExpr = case others of
                          [] -> res'
                          _  -> Letrec (bind (rec others') res')
      changed newExpr

inlineBinders _ _ e = return e

substituteBinders ::
  [LetBinding]
  -> [LetBinding]
  -> Term
  -> ([LetBinding],Term)
substituteBinders [] others res = (others,res)
substituteBinders ((bndr,valE):rest) others res
  = let val   = unembed valE
        res'  = substTm (varName bndr) val res
        rest' = map (second ( embed
                            . substTm (varName bndr) val
                            . unembed)
                    ) rest
        others' = map (second ( embed
                            . substTm (varName bndr) val
                            . unembed)
                    ) others
    in substituteBinders rest' others' res'

localFreeVars ::
  (Functor m, Monad m)
  => Term
  -> RewriteMonad m ([TyName],[TmName])
localFreeVars term = do
  globalBndrs <- fmap (HashMap.keys) $ LabelM.gets bindings
  let (tyFVs,tmFVs) = termFreeVars term
  return (tyFVs,filter (`notElem` globalBndrs) tmFVs)

liftBinders ::
  (Functor m, Monad m)
  => (LetBinding -> RewriteMonad m Bool)
  -> Rewrite m
liftBinders condition ctx expr@(Letrec b) = R $ do
  (xes,res)        <- unbind b
  (replace,others) <- partitionM condition (unrec xes)
  case replace of
    [] -> return expr
    _  -> do
      let (gamma,delta) = contextEnv ctx
      replace' <- mapM (liftBinding gamma delta) replace
      let (others',res') = substituteBinders replace' others res
      let newExpr = case others of
                          [] -> res'
                          _  -> Letrec (bind (rec others') res')
      changed newExpr

liftBinders _ _ e = return e

liftBinding ::
  (Functor m, Monad m)
  => Gamma
  -> Delta
  -> LetBinding
  -> RewriteMonad m LetBinding
liftBinding gamma delta (Id idName tyE,eE) = do
  let ty = unembed tyE
  let e  = unembed eE
  -- Get all local FVs, excluding the 'idName' from the let-binding
  (localFTVs,localFVs) <- localFreeVars e
  let localFVs' = filter (/= idName) localFVs
  -- Abstract expression over its local FVs
  let boundFTVs = map (mkTyVar delta) localFTVs
  let boundFVs  = map (mkId gamma) localFVs'
  let newBody   = mkTyLams (mkLams e boundFVs) boundFTVs
  -- Make a new global ID
  let newBodyTy = termType gamma newBody
  newBodyId <- fmap (makeName (name2String idName) . toInteger) getUniqueM
  -- Make a new expression, consisting of the te lifted function applied to it's free variables
  let newExpr   = mkApps (mkTyApps (Var newBodyId) $ map mkTyVarTy localFTVs) $ map Var localFVs'
  let newBody'  = substTm idName newExpr newBody
  LabelM.modify bindings (HashMap.insert newBodyId (newBodyTy,newBody'))
  return (Id idName (embed ty), embed newExpr)

liftBinding _ _ _ = error $ $(curLoc) ++ "liftBinding: invalid core, expr bound to tyvar"

mkFunction ::
  (Functor m, Monad m)
  => [CoreContext]
  -> TmName
  -> Term
  -> RewriteMonad m TmName
mkFunction ctx bndr body = do
  gamma <- mkGamma ctx
  let bodyTy = termType gamma body
  bodyId <- cloneVar bndr
  addGlobalBind bodyId bodyTy body
  return bodyId

addGlobalBind ::
  (Functor m, Monad m)
  => TmName
  -> Type
  -> Term
  -> RewriteMonad m ()
addGlobalBind vId ty body = LabelM.modify bindings (HashMap.insert vId (ty,body))

cloneVar ::
  (Functor m, Monad m)
  => TmName
  -> RewriteMonad m TmName
cloneVar name = fmap (makeName (name2String name) . toInteger) getUniqueM

isLocalVar ::
  (Functor m, Monad m)
  => Term
  -> RewriteMonad m Bool
isLocalVar (Var name)
  = fmap (not . HashMap.member name)
  $ LabelM.gets bindings
isLocalVar _ = return False

isUntranslatable ::
  (Functor m, Monad m)
  => Term
  -> RewriteMonad m Bool
isUntranslatable _
  = traceIf True ($(curLoc) ++ "isUntranslatable undefined")
  $ return False
