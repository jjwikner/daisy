import uno
import csv
import fileinput
import math
import os
import unohelper

# a UNO struct later needed to create a document
from com.sun.star.text.ControlCharacter import PARAGRAPH_BREAK
from com.sun.star.text.TextContentAnchorType import AS_CHARACTER
from com.sun.star.awt import Size
from com.sun.star.beans import PropertyValue

from com.sun.star.lang import XMain

def insertTextIntoCell( table, cellName, text, color ):
    """Adds new text into a cell in a table. Table is 
    specified by table, cell is given by its name"""
    tableText = table.getCellByName( cellName )
    cursor    = tableText.createTextCursor()
    cursor.setPropertyValue( "CharColor", color )
    tableText.setString( text )

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

def dWrite(doc, cursor, string):
    aHandle = doc.Text.insertString( cursor, string, 0)
    return aHandle
    
def daisyPrintSineTable( doc, cursor):
    wT = 0.1
    text = doc.Text
    for n in range(1, 32):
        text.insertString(cursor, str(math.sin(wT*n*math.pi))+"\n", 0)

def daisyReadCsvFile(doc, cursor, fileName):
    inputFile = csv.reader(open(fileName,'rb'), delimiter=' ', quotechar="|")
    text = doc.Text
    for row in inputFile:
        for col in row:
            text.insertString(cursor, col, 0)
        text.insertString(cursor, "\n", 0)

        
def daisyPrintDirList(doc, cursor):
    text = doc.Text
    for top, dirs, files in os.walk('./'):
        for nm in files:       
            text.insertString(cursor, os.path.join(top, nm), 0)

def daisyCreateTable( doc, cursor, cols, rows):
    # create a text table
    text = doc.Text
    # cursor = text.createTextCursor()
    table = doc.createInstance( "com.sun.star.text.TextTable" )
    table.initialize( rows, cols )
    text.insertTextContent( cursor, table, 0)
    return table

def BR(doc, cursor):
    doc.Text.insertControlCharacter( cursor, PARAGRAPH_BREAK, 0 )


def littleDialog():
  """This is how to call a Basic dialog"""
  psm = uno.getComponentContext().ServiceManager
  dp = psm.createInstance("com.sun.star.awt.DialogProvider")
  dlg = dp.createDialog("vnd.sun.star.script:Standard.Dialog1?location=application")
  dlg.execute()
  return None

def createDoc():
    """creates a new writer document and inserts a table
    with some data (also known as the SWriter sample)""" 

    ctx = uno.getComponentContext()
    smgr = ctx.ServiceManager
    desktop = smgr.createInstanceWithContext( "com.sun.star.frame.Desktop",ctx)
    
    # open a writer document
    doc = desktop.loadComponentFromURL( "private:factory/swriter","_blank", 0, () )
    
    text = doc.Text
    cursor = text.createTextCursor()
    text.insertString( cursor, "The first line in the newly created text document.\n", 0 )
    text.insertString( cursor, "Now we are in the second line\n" , 0 )
    
    # Create a text table
    noRows  = 5
    noCols  = 4
    table = daisyCreateTable(doc, cursor, noCols, noRows)
    rows  = table.Rows

    table.setPropertyValue( "BackTransparent", uno.Bool(0) )
    table.setPropertyValue( "BackColor", 13421823 )

    row = rows.getByIndex(0)
    row.setPropertyValue( "BackTransparent", uno.Bool(0) )
    row.setPropertyValue( "BackColor", 6710932 )

    textColor = 16777215

    insertTextIntoCell( table, "A1", "FirstColumn", textColor )
    insertTextIntoCell( table, "B1", "SecondColumn", textColor )
    insertTextIntoCell( table, "C1", "ThirdColumn", textColor )
    insertTextIntoCell( table, "D1", "SUM", textColor )

    values = ( (22.5,21.5,121.5),
              (5615.3,615.3,-615.3),
              (-2315.7,315.7,415.7) )

    table.getCellByName("A2").setValue(22.5)
    table.getCellByName("B2").setValue(5615.3)
    table.getCellByName("C2").setValue(-2315.7)
    table.getCellByName("D2").setFormula("sum <A2:C2>")

    table.getCellByName("A3").setValue(21.5)
    table.getCellByName("B3").setValue(615.3)
    table.getCellByName("C3").setValue(-315.7)
    table.getCellByName("D3").setFormula("sum <A3:C3>")

    table.getCellByName("A4").setValue(121.5)
    table.getCellByName("B4").setValue(-615.3)
    table.getCellByName("C4").setValue(415.7)
    table.getCellByName("D4").setFormula("sum <A4:C4>")


    cursor.setPropertyValue( "CharColor", 255 )
    cursor.setPropertyValue( "CharShadowed", uno.Bool(1) )

    BR(doc, cursor)
    text.insertString( cursor, " This is a colored Text - blue with shadow\n" , 0 )
    BR(doc, cursor)


    textFrame = doc.createInstance( "com.sun.star.text.TextFrame" )
    textFrame.setSize( Size(15000,400))
    textFrame.setPropertyValue( "AnchorType" , AS_CHARACTER )

    text.insertTextContent( cursor, textFrame, 0 )

    textInTextFrame = textFrame.getText()
    cursorInTextFrame = textInTextFrame.createTextCursor()
    textInTextFrame.insertString( cursorInTextFrame, "The first line in the newly created text frame.", 0 )
    textInTextFrame.insertString( cursorInTextFrame, "\nWith this second line the height of the rame raises.",0)
    BR(doc, cursor)

    cursor.setPropertyValue( "CharColor", 65536 )
    cursor.setPropertyValue( "CharShadowed", uno.Bool(0) )

    daisyImportTextFile(doc, cursor, "/home/jacobw/textfile.txt")

    daisyPrintSineTable(doc, cursor)

    #daisyPrintDirList(doc, cursor)

    text.insertString( cursor, " That's all for now !!", 0 )
    BR(doc, cursor)

    #daisyReadCsvFile(doc, cursor, "/home/jacobw/textfile.txt")

    text.insertString( cursor, "No, it wasnt!!", 0 )
    BR(doc, cursor)

    # Save to file
    properties = ( PropertyValue('Overwrite', 0, True, 0), )

    doc.storeToURL('file:///home/jacobw/test.odt', properties)
    
    #oTables = doc.TextTables
    #text.insertString( cursor, oTables.Length, 0 )

    table.Rows.insertByIndex(table.Rows.getCount(),1)
    table.Rows.insertByIndex(table.Rows.getCount(),1)
    table.Rows.insertByIndex(table.Rows.getCount(),1)

    text.insertString( cursor, str(table.Rows.getCount()), 0)

    table2 = daisyCreateTable(doc, cursor, noCols, noRows)

    # for tableName in doc.TextTables:

    text.insertString( cursor, "There are "+ str(doc.TextTables.getCount()) + " tables in the document", 0)
    BR(doc, cursor)

    dWrite(doc, cursor, "The names of these are:")

    for tableId in doc.TextTables.getElementNames():
        BR(doc, cursor)
        text.insertString( cursor, tableId, 0)
        BR(doc, cursor)

    # littleDialog()



g_exportedScripts = createDoc,
