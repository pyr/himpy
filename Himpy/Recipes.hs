module Himpy.Recipes where
import Himpy.Recipes.Network
import Himpy.Recipes.Storage
import Himpy.Recipes.WinServices
import Himpy.Recipes.Load
import Himpy.Recipes.Juniper
import Himpy.Types

instance RecipeMod HimpyRecipe where
  recipe mod chan logchan host =
    case mod of
         NetworkRecipe       -> net_rcp chan logchan host
         StorageRecipe       -> storage_rcp chan logchan host
         (WinSrvRecipe srvs) -> srv_rcp srvs chan logchan host
         LoadRecipe          -> load_rcp chan logchan host
         JuniperRecipe       -> jun_rcp chan logchan host
