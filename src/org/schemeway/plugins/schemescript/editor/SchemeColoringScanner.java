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
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeColoringScanner implements ITokenScanner {
    private Token defaultToken = new Token(null);
    private Token keywordToken = new Token(null);
    private Token defineToken = new Token(null);
    private Token constantToken = new Token(null);
    private Token specialToken = new Token(null);
    private Token mutatorToken = new Token(null);
    private Token typeToken = new Token(null);
    private Token errorToken = new Token(null);

    private SchemeScanner mScanner = new SchemeScanner();
    private ColorManager mColorManager;
    private KeywordManager mKeywordManager;

    public SchemeColoringScanner(ColorManager manager, KeywordManager keywordManager) {
        mColorManager = manager;
        mKeywordManager = keywordManager;
        updateColors();
    }

    public void updateColors() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        defaultToken.setData(new TextAttribute(findColor(store, ColorPreferences.DEFAULT_COLOR)));
        defineToken.setData(new TextAttribute(findColor(store, ColorPreferences.DEFINE_COLOR), null, SWT.BOLD));
        keywordToken.setData(new TextAttribute(findColor(store, ColorPreferences.KEYWORD_COLOR), null, SWT.BOLD));
        specialToken.setData(new TextAttribute(findColor(store, ColorPreferences.SPECIAL_COLOR), null, SWT.BOLD));
        constantToken.setData(new TextAttribute(findColor(store, ColorPreferences.CONSTANT_COLOR)));
        mutatorToken.setData(new TextAttribute(findColor(store, ColorPreferences.MUTATOR_COLOR), null, SWT.BOLD));
        typeToken.setData(new TextAttribute(findColor(store, ColorPreferences.TYPE_COLOR)));
        errorToken.setData(new TextAttribute(findColor(store, ColorPreferences.ERROR_COLOR)));
        PreferenceUtil.updateKeywordManager(store, mKeywordManager);
    }

    private Color findColor(IPreferenceStore store, String name) {
        RGB rgb = PreferenceConverter.getColor(store, name);
        return mColorManager.getColor(rgb);
    }

    private int mOffset = -1;
    private int mLength = -1;

    public IToken nextToken() {
        SchemeToken token = mScanner.nextToken();
        if (token == SchemeToken.EOF)
            return Token.EOF;

        mOffset = token.getOffset();
        mLength = token.getLength();

        switch (token.getType()) {
            case SchemeToken.CONSTANT:
                return constantToken;
            case SchemeToken.SYMBOL:
            {
                String text = mScanner.getText(mOffset, mLength);
                if (isSchemeType(text))
                    return typeToken;
                
                String category = mKeywordManager.getType(text);
                if (category == KeywordManager.TYPE_OTHER)
                    return defaultToken;
                if (category == KeywordManager.TYPE_DEFINE)
                    return defineToken;
                if (category == KeywordManager.TYPE_KEYWORD)
                    return keywordToken;
                if (category == KeywordManager.TYPE_MUTATOR)
                    return mutatorToken;
                if (category == KeywordManager.TYPE_SPECIAL)
                    return specialToken;
            }
            case SchemeToken.SPECIAL:
            {
                String category = mKeywordManager.getType(mScanner.getText(mOffset, mLength));
                if (category == KeywordManager.TYPE_KEYWORD)
                    return keywordToken;
                if (category == KeywordManager.TYPE_CONSTANT)
                    return constantToken;
                return errorToken;
            }
            case SchemeToken.ERROR:
            {
                return errorToken;
            }
        }
        return defaultToken;
    }

    private boolean isSchemeType(String text) {
        return mLength > 2 && text.charAt(0) == '<' && text.charAt(mLength - 1) == '>';
    }

    public int getTokenOffset() {
        return mOffset;
    }

    public int getTokenLength() {
        return mLength;
    }

    public void setRange(IDocument document, int offset, int length) {
        mScanner.setRange(document, offset, length);
    }
}