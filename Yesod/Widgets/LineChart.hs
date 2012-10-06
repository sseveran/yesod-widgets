{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
module Yesod.Widgets.LineChart(Chart(..), buildLineChart)where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Yesod

data Yesod a => Chart a = Chart {
  cColors :: [Text],
  cDataRoute :: Route a, 
  cElementId :: Text,
  cSeries :: [(Int,Text)]
}

buildLineChart :: Yesod a => Route a -> Chart a -> GWidget a a ()
buildLineChart flotRoute chart = do
    let widget = do
          addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
          addScript flotRoute
          toWidget [julius| 
            $(document).ready(function(){
              $.getJSON('@{cDataRoute chart}', function(data){
                $.plot($('##{cElementId chart}'),#{renderedSeries $ cSeries chart}, 
                  { colors:#{renderedColors $ cColors chart},
                    xaxis: { 
                      minTickSize: [1, "hour"],
                      mode: "time",
                      timeformat: "%y/%m/%d %h" 
                      },
                    grid:{
                      clickable:true,
                      hoverable:true
                    }
                  });
              })
            })

      |] 
    widget
  where
    renderedColors :: [Text] -> Text
    renderedColors c = renderJS $ L.map (\ x -> T.concat ["\"",x,"\""] ) c
  
    renderedSeries :: [(Int,Text)] -> Text
    renderedSeries series = renderJS $ L.map (\ x -> T.concat ["{data:data[",T.pack $ show $ fst x,"],label:\"", snd x, "\"}"]) series

    renderJS js = T.concat ["[", T.intercalate "," js,"]"]
