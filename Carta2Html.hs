module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder



{-
ghc --make Carta2Html.hs -o carta2Html
-}

options = 
	[ "molto"
	, "ank"
	, "wajaca"
	]

main = do 
	initGUI
	b <- builderNew
	builderAddFromFile b "carta2html.glade"
	let getWidget = builderGetObject b
	window <- getWidget castToWindow "window1"
	label <- builderGetObject b castToLabel "label1"
	styleComboBox <- builderGetObject b castToComboBox "styleComboBox"
	wrappingCheckButton <- builderGetObject b castToToggleButton "wrappingCheckButton"
	gridCheckButton <- builderGetObject b castToToggleButton "gridCheckButton"
	columnSpinButton <- builderGetObject b castToSpinButton "columnSpinButton"
	filepath <- builderGetObject b castToEntry "fileEntry"
	closeButton <- builderGetObject b castToButton "closeButton"
	processButton <- builderGetObject b castToButton "processButton"
	onClicked closeButton $ do 
		putStrLn "closeButton clicked"
		widgetDestroy window
	onClicked processButton $ do 
		name <- entryGetText filepath :: IO String      
		cols <- spinButtonGetValueAsInt columnSpinButton
		shouldWrap <- toggleButtonGetActive wrappingCheckButton
		styleOption <- comboBoxGetActive styleComboBox
		print styleOption
		set label [labelText := "Hello " ++ name]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI