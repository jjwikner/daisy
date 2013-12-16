import uno
#import  daisyOoops as daipy

from com.sun.star.awt import Size
from com.sun.star.beans import PropertyValue


def daisyAlignFrames(currentDoc, textWidth=11000, color = 0):
    textPtr = currentDoc.Text
    frameStyle = "Frame"
    for i in range(0, currentDoc.TextFrames.getCount()):
        activeFrame = currentDoc.TextFrames.getByIndex(i)
        activeFrame.setPropertyValue( "FrameStyleName", frameStyle)
        activeFrame.setSize(Size(textWidth, 1))
        activeFrame.setPropertyValue( "BackColor", color)
        activeFrame.setPropertyValue( "FrameIsAutomaticHeight", True )


def daisyFrames():
    currentDoc = XSCRIPTCONTEXT.getDocument()
     
    daisyAlignFrames(currentDoc, 10000, 0*255*255)
    
g_exportedScripts = daisyFrames,
