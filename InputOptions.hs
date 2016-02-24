module InputOptions ( processOptionsWith
	                , processOptionsWith'
	                , CartaOptions (..)
	                , StyleSelector (..)
	                , GridSys (..)
	                , PreWrapped (..)) where 

import Options.Applicative

data CartaOptions = CartaOptions String StyleSelector PreWrapped Columns GridSys SectionName deriving (Show,Eq)

type SectionName = String

data StyleSelector = StyleSelector String deriving (Show,Eq)

type Columns = Int
type FileName = String 

data PreWrapped = Wrapped | NotWrapped deriving (Show,Eq)

readInt = read :: String -> Int

data GridSys = Bootstrap | Foundation deriving (Show,Eq)

inputFile :: Parser FileName 
inputFile = 
	     strOption (long "input" 
	  	            <> short 'i' <> metavar "Archivo CSV" 
	  	            <> help "Nombre del archivo a procesar") 

sectionTitle :: Parser SectionName 
sectionTitle = 
	        strOption (long "title" 
	  	            <> short 't' <> metavar "Titulo Seccion" 
	  	            <> help "Titulo de la seccion de la carta")


wrappingOption :: Parser PreWrapped 
wrappingOption = 
	flag NotWrapped Wrapped
     ( long "wrap"
     <> short 'w'
     <> help "Procesar envolviendo el contenido")

gridSystem :: Parser GridSys 
gridSystem = 
	flag Bootstrap Foundation
     ( long "gridSystem"
     <> short 'g'
     <> help "Systema de columnas Bootstrap/Foundation")

styleSelector :: Parser StyleSelector 
styleSelector = 
	StyleSelector 
	  <$> strOption (long "style" 
	  	            <> short 's' <> metavar "Estilo" 
	  	            <> help "Estilo de carta seleccionado")

layoutColumns :: Parser Columns 
layoutColumns = 
	readInt 
	  <$> strOption (long "cols" 
	  	            <> short 'c' <> metavar "Columnas" 
	  	            <> help "Numero de columnas del layout") <|> (pure 0)


optionParser :: Parser CartaOptions
optionParser = CartaOptions <$> inputFile 
                            <*> styleSelector <*> wrappingOption 
                            <*> layoutColumns <*> gridSystem 
                            <*> sectionTitle

--processOptionsWith :: (CartaOptions -> IO b) -> IO b
processOptionsWith sl f = execParser (opts sl) >>= f
        where
		  opts styleList = info (helper <*> optionParser)
		     (fullDesc 
		     	<> progDesc ("Genera HTML para cartas dado un archivo CSV \n opciones disponibles:" 
		     		         ++ unlines styleList) 
		     	<> header "Cartas HTML - Daniel Cardona Rojas 2016")

processOptionsWith' = processOptionsWith []

main = processOptionsWith' print