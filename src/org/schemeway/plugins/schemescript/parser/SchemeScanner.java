/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import org.eclipse.jface.text.*;

public class SchemeScanner {

    public SchemeScanner() {
    }

    private IDocument mDocument = null;

    private int mRangeStart = -1;
    private int mRangeEnd = -1;
    private int mTokenStart = -1;
    private int mTokenEnd = -1;

    private final static char EOR = (char) -1; // end of range character

    public int getTokenLength() {
        return mTokenEnd - mTokenStart;
    }

    public int getTokenOffset() {
        return mTokenStart;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.rules.ITokenScanner#nextToken() TODO -
     *      support quote, unquote, quasiquote, unquote-splicing, constant
     *      vectors TODO - support #e..., #i..., #o..., #b..., #d....
     */
    public SchemeToken nextToken() {
        // start a new token
        mTokenStart = mTokenEnd;
        try {
            char ch = lookahead();
            if (ch == EOR)
                return SchemeToken.EOF;

            switch (ch) {
                case '[':
                {
                    if (SchemeScannerUtilities.bracketsAreParentheses()) {
                        consume();
                        return SchemeToken.createLeftParen(getTokenOffset());
                    }
                    else
                        return parseDefaultToken(ch);
                }
                case '(':
                {
                    consume();
                    return SchemeToken.createLeftParen(getTokenOffset());
                }
                case ']':
                {
                    if (SchemeScannerUtilities.bracketsAreParentheses()) {
                        consume();
                        return SchemeToken.createRightParen(getTokenOffset());
                    }
                    else
                        return parseDefaultToken(ch);
                }
                case ')':
                {
                    consume();
                    return SchemeToken.createRightParen(getTokenOffset());
                }
                case '#':
                {
                    return parseDashPrefixedToken();
                }
                case ' ':
                case '\t':
                case '\n':
                case '\r':
                {
                    return parseWhitespace();
                }
                case '\'':
                {
                    consume();
                    return SchemeToken.createQuote(getTokenOffset());
                }
                case '`':
                {
                    consume();
                    return SchemeToken.createBackquote(getTokenOffset());
                }
                case ',':
                {
                    consume();
                    ch = lookahead();
                    if (ch != EOR && ch == '@') {
                        consume();
                        return SchemeToken.createUnquoteSplicing(getTokenOffset());
                    }
                    else
                        return SchemeToken.createUnquote(getTokenOffset());
                }
                default:
                    return parseDefaultToken(ch);
            }
        }
        catch (BadLocationException exception) {
        }
        return SchemeToken.EOF;
    }

    private SchemeToken parseDefaultToken(char ch) throws BadLocationException {
        if (SchemeScannerUtilities.isIdentifierPartChar(ch)) {
            consume();
            ch = lookahead();
            while (ch != EOR && SchemeScannerUtilities.isIdentifierPartChar(ch)) {
                consume();
                ch = lookahead();
            }
            return SchemeToken.createSymbol(getTokenOffset(), getTokenLength());
        }
        else {
            consume();
            return SchemeToken.createDefault(getTokenOffset(), getTokenLength());
        }
    }

    private SchemeToken parseWhitespace() throws BadLocationException {
        consume();
        while (SchemeScannerUtilities.isWhitespaceChar(lookahead()))
            consume();
        return SchemeToken.createWhitespace(getTokenOffset(), getTokenLength());
    }

    private SchemeToken parseDashPrefixedToken() throws BadLocationException {
        char ch;
        consume();
        ch = lookahead();
        switch (ch) {
            case 't':
            case 'f':
            {
                consume();
                return SchemeToken.createConstant(getTokenOffset(), getTokenLength());
            }
            case '!':
            {
                return parseSpecialKeyword();
            }
            case '\\':
            {
                return parseCharacterToken();
            }
            case '(':
            {
                return SchemeToken.createVectorPrefix(getTokenOffset());
            }
            default:
                return parseDefaultToken(ch);
                //return SchemeToken.createError(getTokenOffset(), getTokenLength());
        }
    }

    // -- Helpers

    private SchemeToken parseCharacterToken() throws BadLocationException {
        char ch;
        consume();
        ch = lookahead();
        if (Character.isLetter(ch)) {
            consume();
            while (Character.isLetter(lookahead())) {
                consume();
            }
            return SchemeToken.createConstant(getTokenOffset(), getTokenLength());
        }
        else
            if (ch != EOR && !(SchemeScannerUtilities.isWhitespaceChar(ch))) {
                consume();
                return SchemeToken.createConstant(getTokenOffset(), getTokenLength());
            }
            else
                return SchemeToken.createError(getTokenOffset(), getTokenLength());
    }

    private SchemeToken parseSpecialKeyword() throws BadLocationException {
        consume();
        if (Character.isLetter(lookahead())) {
            consume();
            while (Character.isLetter(lookahead())) {
                consume();
            }
            return SchemeToken.createSpecial(getTokenOffset(), getTokenLength());
        }
        else
            return SchemeToken.createError(getTokenOffset(), getTokenLength());
    }

    private final void consume() {
        if (mTokenEnd < mRangeEnd)
            mTokenEnd++;
    }

    private final char lookahead() throws BadLocationException {
        return (mTokenEnd < mRangeEnd) ? mDocument.getChar(mTokenEnd) : EOR;
    }

    public String getText(int offset, int length) {
        try {
            return mDocument.get(offset, length);
        }
        catch (BadLocationException exception) {
            return "";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.rules.ITokenScanner#setRange(org.eclipse.jface.text.IDocument,
     *      int, int)
     */
    public void setRange(IDocument document, int offset, int length) {
        mDocument = document;
        mRangeStart = offset;
        mRangeEnd = offset + length;
        mTokenStart = mTokenEnd = mRangeStart;
    }
}