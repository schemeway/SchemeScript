/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.preference.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.schemeway.plugins.schemescript.*;

public class SchemeDefaultScanner implements ITokenScanner {
    private ColorManager mColorManager;
    private String mColorName;
    private Token mToken = new Token(null);
    private int mCurrentOffset;
    private int mCurrentLength;
    private boolean mEmpty;

    public SchemeDefaultScanner(ColorManager manager, String colorName) {
        mColorManager = manager;
        mColorName = colorName;
        updateColors();
    }

    public void updateColors() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();

        RGB rgb = PreferenceConverter.getColor(store, mColorName);
        boolean bold = store.getBoolean(mColorName + ".bold");
        boolean ital = store.getBoolean(mColorName + ".italic");
        int style = SWT.NONE;
        if (bold) style |= SWT.BOLD;
        if (ital) style |= SWT.ITALIC;
        mToken.setData(new TextAttribute(mColorManager.getColor(rgb), null, style));
    }

    public int getTokenLength() {
        return mCurrentLength;
    }

    public int getTokenOffset() {
        return mCurrentOffset;
    }

    public IToken nextToken() {
        if (mEmpty) {
            return Token.EOF;
        }
        else {
            mEmpty = true;
            return mToken;
        }
    }

    public void setRange(IDocument document, int offset, int length) {
        mCurrentOffset = offset;
		mCurrentLength = length;
		mEmpty = false;
    }
}