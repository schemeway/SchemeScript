/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;

public class SchemeTextHover implements ITextHover {

    public SchemeTextHover() {
        super();
    }

    public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
        if (hoverRegion == null)
            return null;
        
        try {
            String symbol = textViewer.getDocument().get(hoverRegion.getOffset(), hoverRegion.getLength());
            SymbolEntry[] entries = SchemeScriptPlugin.getDefault().getDictionary().findSymbol(symbol);
            if (entries.length == 0) {
                return null;
            }
            return entries[0].getDescription();
        }
        catch (BadLocationException exception) {
            return null;
        }
        
    }

    public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
        try {
            return SchemeTextUtilities.findSymbolRegionAroundPoint(textViewer, offset);
        }
        catch (BadLocationException exception) {
            return null;
        }
    }
}
