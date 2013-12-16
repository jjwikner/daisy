from com.sun.star.table import BorderLine

# helper function
def getNewString( theString ) :
    if( not theString or len(theString) ==0) :
        return ""
    # should we tokenize on "."?
    if theString[0].isupper() and len(theString)>=2 and theString[1].isupper() :
	# first two chars are UC => first UC, rest LC
        newString=theString[0:1].upper() + theString[1:].lower();
    elif theString[0].isupper():
	# first char UC => all to LC
        newString=theString.lower()
    else: # all to UC.
        newString=theString.upper()
    return newString;

def daisyImportTextFile( doc, cursor, fileName ): 
    """Takes a text file and prints it in the document"""

    try:
        file = open(fileName)
    except:
        file = False
            
    text = doc.Text
    while file:
        line = file.readline()
        if not line:
            break
        pass             
        text.insertString( cursor, line, 0)

def daisyCreateTable( doc, cursor, cols, rows):
    # create a text table
    text = doc.Text
    # cursor = text.createTextCursor()
    table = doc.createInstance( "com.sun.star.text.TextTable" )
    table.initialize( rows, cols )
    text.insertTextContent( cursor, table, 0)
    return table

def insertTextIntoCell( table, cellName, text, color ):
    """Adds new text into a cell in a table. Table is 
    specified by table, cell is given by its name"""
    tableText = table.getCellByName( cellName )
    cursor    = tableText.createTextCursor()
    cursor.setPropertyValue( "CharColor", color )
    tableText.setString( text )
    
def dWrite(doc, cursor, string):
    aHandle = doc.Text.insertString( cursor, string, 0)
    return aHandle

def insertFile( ): 

    """Load and insert the text file provided the
    file name copied from the document"""

    import string

    doc = XSCRIPTCONTEXT.getDocument()
    
    # Pick up the selection
    selection = doc.getCurrentController().getSelection()

    # This could be a list of several words, so pick the first one
    firstWord = selection.getByIndex(0);

    # Extract the actual string
    theString  = firstWord.getString();

    # And the text in it
    xText = firstWord.getText();

    # And a pointer for where the text is.
    xWordCursor = xText.createTextCursorByRange(firstWord);

    table = daisyCreateTable(doc, xWordCursor, 1, 2)
    textColor = 0
    
    insertTextIntoCell( table, "A1", "File name:" + theString, textColor )
    insertTextIntoCell( table, "A2", "... and here we could paste the file contents ...", textColor )

    
    # Load and add the file to the text
    daisyImportTextFile( doc, xWordCursor, theString )


    dWrite(doc, xWordCursor, table.getCellByName("A1").getType())
    #table.getCellByName("A1").getType()

    #tableBorder = table.getPropertyValue("TableBorder")
    #borderLine  = BorderLine()
    #borderLine.OuterLineWidth = 120
    #tableBorder.BottomLine = borderLine

g_exportedScripts = insertFile,
