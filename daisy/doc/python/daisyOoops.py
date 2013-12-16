#!/usr/bin/python
# Filename: daisyOoops.py

import uno
import csv
import fileinput
import math
import os
import wx

# a UNO struct later needed to create a document
from com.sun.star.text.ControlCharacter import PARAGRAPH_BREAK
from com.sun.star.text.TextContentAnchorType import AS_CHARACTER
from com.sun.star.awt   import Size
from com.sun.star.beans import PropertyValue
from com.sun.star.lang  import XMain

def insertTextIntoCell( table, cellName, text, color ):
    """Adds new text into a cell in a table. Table is 
    specified by table, cell is given by its name"""
    tableText = table.getCellByName( cellName )
    #tableText = table.getCellByPosition(1,2)
    cursor    = tableText.createTextCursor()
    cursor.setPropertyValue( "CharColor", color )
    tableText.setString( text )

def br(doc, cursor):
    doc.Text.insertControlCharacter( cursor, PARAGRAPH_BREAK, 0 )

def daisyDocProp( property ):
    """To simplify a bit, we catch the 
    document every time in this function"""
    try: 
        propValue = XSCRIPTCONTEXT.getDocument().getDocumentProperties().getUserDefinedProperties().getPropertyValue(property)
    except:
        propValue = "<UNKNOWN>"
    return propValue

def daisyIncrRev(revision, majorNotMinor):
    """Add some intelligent way of incrementing
    the revision"""
    Rev = ""
    return Rev

def daisyDocPtr():
    # ctx = uno.getComponentContext()
    # smgr = ctx.ServiceManager
    
    # Here we could have a flag also, such that we could point to an url, open it, etc.
    # or just open a new one, or take the active one.
    doc = XSCRIPTCONTEXT.getDocument()
    return doc

def pr(textPtr, cursor, stringToPut):
    """Inserts a string into the text, but we 
    need to add a flag if it is body text or table text"""
    textPtr.insertString(cursor, stringToPut, 0)


    
def daisyAlignFrames(currentDoc, textWidth=17000, color = 0):
    textPtr = currentDoc.Text
    frameStyle = "Frame"
    for i in range(0, currentDoc.TextFrames.getCount()):
        activeFrame = currentDoc.TextFrames.getByIndex(i)
        activeFrame.setPropertyValue( "FrameStyleName", frameStyle)
        activeFrame.setSize(Size(textWidth, 1))
        activeFrame.setPropertyValue( "BackColor", color)
        activeFrame.setPropertyValue( "FrameIsAutomaticHeight", True )


