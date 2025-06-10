#!/bin/bash

pt_cld_path="/home/alex/local-usask/external-data/wolf-creek/menounos/Aug2018_WolfCreek_class.laz"

shp_clip_new="/home/alex/local-usask/working-papers/model-eval-paper/data/wolf-creek/snow_survey/wolf-creek-snow-survey-rough.shp"

out_path="/home/alex/local-usask/working-papers/model-eval-paper/data/wolf-creek/snow_survey/"

/home/alex/bin/LAStools/bin/lasclip64 -i $pt_cld_path \
        -poly $shp_clip_new \
        -odir "$out_path" \
        -odix "_clip_snow_survey_rough" \
        -olas \
        -v
