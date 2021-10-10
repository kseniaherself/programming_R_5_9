# shtuka -----------------------------------------------------------------------
library(lingtypology)
map.feature(lang.aff("Circassian"))

df <- data.frame(language = c("Adyghe", "Kabardian", "Polish", "Russian", "Bulgarian"),
                 features = c("polysynthetic", "polysynthetic", "fusional", "fusional", "fusional"))
df$popup <- c ("sɐ s-ɐ-k'ʷɐ<br> 1sg 1sg.abs-dyn-go<br>'I go'",
               "sɐ s-o-k'ʷɐ<br> 1sg 1sg.abs-dyn-go<br>'I go'",
               "id-ę<br> go-1sg.npst<br> 'I go'",
               "ya id-u<br> 1sg go-1sg.npst <br> 'I go'",
               "id-a<br> go-1sg.prs<br> 'I go'")
# create a map
map.feature(df$language, df$features, df$popup)
map.feature(languages = df$language,
            popup = df$popup, 
            features = df$features, 
            title = "NEW MAP !!!!")

# tell me more: https://github.com/agricolamz/lingtypology/wiki
