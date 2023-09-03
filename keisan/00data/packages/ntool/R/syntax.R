# by naoi okada
# charset=utf-8
# 
# title:
#   Generate CotEditor's Syntax
# description:
#   CotEditor用のシンタックスファイルを生成する。
syntax <- function () {
	xml <- list()
	xml <- within(xml, extensions <- "<key>keyString</key><string>R</string>")
	xml <- within(xml, charactersArray <- list())
	xml <- within(xml, commandsArray <- ls("package:base", all.names = TRUE, pattern = "^[[:alpha:].][[:alnum:]_.]*$"))
	xml <- within(xml, commentsArray <- "#.*")
	xml <- within(xml, keywordsArray <- strsplit("if else repeat while function for in next brewk", " ")[[1]])
	xml <- within(xml, numbersArray <- c("\\b(\\d+\\.?|(\\d+)?\\.\\d+)(e[+-]?\\d+)?L?\\b", "\\b0x[\\da-fA-F]+\\b"))
	xml <- within(xml, outlineMenuArray <- c("^([[:blank:]]*)(.+?)[[:blank:]]*&lt;-[\\t ]*function $1$2"))
	xml <- within(xml, stringsArray <- c("\"", "\'"))
	xml <- within(xml, valuesArray <- strsplit("TRUE FALSE NULL Inf NaN NA NA_integer_ NA_real_ NA_complex_ NA_character_ ...", " ")[[1]])
	xml <- lapply(xml, sapply, gsub, pattern = "^([\"'])$", replacement = "<key>beginString</key><string>\\1</string><key>endString</key><string>\\1</string>")
	xml <- lapply(xml, sapply, gsub, pattern = "^([[:alpha:].][[:alnum:]_.]*)$", replacement = "<key>beginString</key><string>\\1</string>")
	xml <- lapply(xml, sapply, gsub, pattern = "^(.*[*+].*)$", replacement = "<key>beginString</key><string>\\1</string><key>regularExpression</key><true/>")
	xml <- lapply(xml, sapply, gsub, pattern = "^(.*) ([^<]*)(</.*)$", replacement = "\\1\\3<key>keyString</key><string>\\2</string>")
	xml <- lapply(xml, sapply, gsub, pattern = "(<[[:alpha:]])", replacement = "\n\t\t\t\t\\1")
	xml <- lapply(xml, sprintf, fmt = "\t\t\t<dict>%s\n\t\t\t</dict>")
	xml <- mapply(c, sprintf("\t\t<key>%s</key>", names(xml)), "\t\t<array>", xml, "\t\t</array>")
	xml <- unlist(xml, use.names = FALSE)
	xml <- append(xml, "\t\t<key>sytleName</key>\n\t\t<string>R</string>", 0)
	xml <- append(xml, sprintf("\t\t<key>version</key>\n\t\t<string>%s</string>", package_version(version)), 1) 
	xml <- append(xml, "<plist version=\"1.0\">\n\t<dict>", 0)
	xml <- append(xml, "\t</dict>\n</plist>")
	xml <- append(xml, "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">", 0)
	xml <- append(xml, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 0)
	xml <- writeLines(xml, path.expand("~/Library/Application Support/CotEditor/SyntaxColorings/R.plist"))
	invisible()
}
