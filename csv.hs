import Text.ParserCombinators.Parsec

-- csv-file       = { row }
csv_file = many row

-- row            = field-list, eol
row = do result <- field_list
	 eol
	 return result

-- field-list     = field, [ ",", field-list ]
-- field_list = do first_cell <- field
-- 		comma <- optionMaybe (string ",")
-- 		remaining <- case comma of
-- 		     Just "," -> field_list
-- 		     Nothing -> return []
-- 	        return (first_cell:remaining)
field_list = field `sepBy` (char ',')

-- field          = [ whitespace ], field-value, [ whitespace ]
field = do optional whitespace
	   result <- field_value
	   optional whitespace
	   return result

-- field-value    = quoted-string | bare-string
field_value = quoted_string <|> bare_string

-- quoted-string  = '"', quoted-content, '"'
quoted_string = do char '"'
		   result <- quoted_content
		   char '"'
		   return result

-- quoted-content = { quoted-char }
quoted_content = many quoted_char

-- quoted-char    = (any char except '"' or eol)
quoted_char = noneOf "\"\n"

-- bare-string    = { bare-char }
bare_string = many bare_char

-- bare-char      = (any char except ',' or eol)
bare_char = noneOf ",\n"

-- whitespace     = space-char, { space-char }
whitespace = many space_char

-- space-char     = " " | "\t"
space_char = char ' ' <|> char '\t'

-- eol            = "\n"
eol = string "\n"

main = do c <- getContents
	  case parse csv_file "<stdin>" c of
	       Left e -> do putStr "Error: "
	       		    print e
	       Right rslt -> mapM_ print rslt
