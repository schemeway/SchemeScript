/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.dictionary.*;

public class SchemeTextHover implements ITextHover {
    private SchemeEditor mEditor;
    
    
    public SchemeTextHover(SchemeEditor editor) {
        super();
        mEditor = editor;
    }
    
    protected SchemeEditor getEditor() {
        return mEditor;
    }

    public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
        if (hoverRegion == null)
            return null;
        
        try {
            String symbol = textViewer.getDocument().get(hoverRegion.getOffset(), hoverRegion.getLength());
            SymbolEntry[] entries = getEditor().getSymbolDictionary().findSymbol(symbol);
            if (entries.length == 0) {
                return null;
            }
            StringBuffer buffer = new StringBuffer();
            buffer.append(entries[0].getDescription());
            for (int i=1; i<entries.length; i++) {
                buffer.append('\n');
                buffer.append(entries[i].getDescription());
            }
            return buffer.toString();
        }
        catch (BadLocationException exception) {
            return null;
        }
        
    }

    public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
        try {
            return SchemeTextUtilities.findSymbolRegionAroundPoint(textViewer.getDocument(), offset);
        }
        catch (BadLocationException exception) {
            return null;
        }
    }
}
