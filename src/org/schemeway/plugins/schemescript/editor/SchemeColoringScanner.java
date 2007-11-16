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
    private Token keyToken = new Token(null);
    private Token defineToken = new Token(null);
    private Token constantToken = new Token(null);
    private Token specialToken = new Token(null);
    private Token mutatorToken = new Token(null);
    private Token typeToken = new Token(null);
    private Token errorToken = new Token(null);
    private Token parenToken = new Token(null);
    private Token punctuationToken = new Token(null);

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
        defaultToken.setData(makeAttribute(store, ColorPreferences.DEFAULT_COLOR));
        defineToken.setData(makeAttribute(store, ColorPreferences.DEFINE_COLOR));
        keywordToken.setData(makeAttribute(store, ColorPreferences.KEYWORD_COLOR));
        keyToken.setData(makeAttribute(store, ColorPreferences.KEY_COLOR));
        specialToken.setData(makeAttribute(store, ColorPreferences.SPECIAL_COLOR));
        constantToken.setData(makeAttribute(store, ColorPreferences.CONSTANT_COLOR));
        mutatorToken.setData(makeAttribute(store, ColorPreferences.MUTATOR_COLOR));
        typeToken.setData(makeAttribute(store, ColorPreferences.TYPE_COLOR));
        errorToken.setData(makeAttribute(store, ColorPreferences.ERROR_COLOR));
        parenToken.setData(makeAttribute(store, ColorPreferences.PAREN_COLOR));
        punctuationToken.setData(makeAttribute(store, ColorPreferences.PUNCTUATION_COLOR));
        PreferenceUtil.updateKeywordManager(store, mKeywordManager);
    }

    private TextAttribute makeAttribute(IPreferenceStore store, String name) {
        Color color = findColor(store, name);
        boolean bold = store.getBoolean(name + ".bold");
        boolean italic = store.getBoolean(name + ".italic");
        return makeAttribute(color, bold, italic);
    }
    
    private TextAttribute makeAttribute(Color color, boolean bold, boolean italic) {
        int style = SWT.NORMAL;
        if (bold)
            style |= SWT.BOLD;
        if (italic)
            style |= SWT.ITALIC;
        return new TextAttribute(color, null, style);
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
            case SchemeToken.LPAREN:
            case SchemeToken.RPAREN:
                return parenToken;
            case SchemeToken.EXPR_COMMENT_PREFIX:
            	return errorToken;
            case SchemeToken.BACKQUOTE:
            case SchemeToken.QUOTE:
            case SchemeToken.DOT:
            case SchemeToken.UNQUOTE:
            case SchemeToken.UNQUOTE_SPLICING:
            	return punctuationToken;
            case SchemeToken.SYMBOL:
            {
                String text = mScanner.getText(mOffset, mLength);
                if (isSchemeType(text))
                    return typeToken;
                if (isSchemeKey(text))
                    return keyToken;

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

    private boolean isSchemeKey(String text) {
        int length = text.length();
        if (length == 1)
            return false;
        if (text.charAt(0) == ':' && text.lastIndexOf(':') == 0)
            return true;
        if (text.charAt(length - 1) == ':' && (text.indexOf(':') == (length - 1)))
            return true;
        return false;
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