module System.Himpy.Recipes where
import System.Himpy.Recipes.Network
import System.Himpy.Recipes.Storage
import System.Himpy.Recipes.WinServices
import System.Himpy.Recipes.Iostat
import System.Himpy.Recipes.Load
import System.Himpy.Recipes.Juniper
import System.Himpy.Types

instance RecipeMod HimpyRecipe where
  recipe mod chan logchan host index =
    case mod of
         NetworkRecipe       -> net_rcp chan logchan host index
         StorageRecipe       -> storage_rcp chan logchan host
         (WinSrvRecipe srvs) -> srv_rcp srvs chan logchan host
         LoadRecipe          -> load_rcp chan logchan host
         IostatRecipe        -> iostat_rcp chan logchan host
         JuniperRecipe       -> jun_rcp chan logchan host
