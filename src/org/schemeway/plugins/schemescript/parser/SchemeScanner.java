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
     * TODO - support constants 
     * TODO - support #e..., #i..., #o..., #b..., #d....
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
                    return parsePoundPrefixedToken();
                }
                case ';':
                {
                    return parseComment(false);
                }
                case '"':
                {
                    return parseString();
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
            if (getTokenLength() == 1 && mDocument.getChar(mTokenStart) == '.')
            	return SchemeToken.createDot(getTokenOffset());
            else
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

    private SchemeToken parseComment(boolean multiline) throws BadLocationException {
        if (multiline) {
            // we assume that the leading '#|' has been read and consumed
            boolean vbarSeen = false;
            char ch = lookahead();
            while (ch != EOR) {
                consume();
                if (ch == '#' && vbarSeen) {
                    break;
                }
                else if (ch == '|') {
                    vbarSeen = true;
                }
                else {
                    vbarSeen = false;
                }
                ch = lookahead();
            }
            if (ch != EOR)
                return SchemeToken.createComment(getTokenOffset(), getTokenLength());
            else
                return SchemeToken.createError(getTokenOffset(), getTokenLength());
        }
        else {
            // we assume that the leading ';' has been seen
            consume();
            char ch = lookahead();
            while (ch != EOR && ch != '\r' && ch != '\n') {
                consume();
                ch = lookahead();
            }
            return SchemeToken.createComment(getTokenOffset(), getTokenLength());
        }
    }

    private SchemeToken parseString() throws BadLocationException {
        // we assume that the leading '"' has been read
        consume();
        boolean backslashSeen = false;
        char ch = lookahead();
        while (ch != EOR) {
            consume();
            if (ch == '"' && !backslashSeen) {
                break;
            }
            else if (ch == '\\' && !backslashSeen) {
                backslashSeen = true;
            }
            else 
                backslashSeen = false;
            ch = lookahead();
        }
        if (ch != EOR)
            return SchemeToken.createString(getTokenOffset(), getTokenLength());
        else
            return SchemeToken.createError(getTokenOffset(), getTokenLength());
    }


	private SchemeToken parseHereString() throws BadLocationException {
		consume();
		if (lookahead() == '<') {
			consume();
			String tag = readToEndOfLine();
			String nextLine = readToEndOfLine();
			while (nextLine != null && !nextLine.equals(tag)) {
				nextLine = readToEndOfLine();
			}

			return SchemeToken.createString(getTokenOffset(), getTokenLength());
		}
		else {
			return SchemeToken.createError(getTokenOffset(), getTokenLength());
		}
	}

	
	private String readToEndOfLine() {
		int startPosition = getPosition();
		if (isEndPosition(startPosition)) {
			return null;
		}
		try {
			IRegion lineInfo = mDocument.getLineInformationOfOffset(startPosition);
			int length = lineInfo.getLength() - (startPosition - lineInfo.getOffset());
			mTokenEnd = startPosition + length;
			char ch = lookahead();
			while (ch == '\n' || ch == '\r') {
				consume();
				ch = lookahead();
			}
			return mDocument.get(startPosition, length);

		}
		catch (BadLocationException e) {
			return null;
		}

	}

    private SchemeToken parsePoundPrefixedToken() throws BadLocationException {
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
            case ';': 
            {
            	consume();
            	return SchemeToken.createSexprCommentPrefix(getTokenOffset());
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
            case '<':
            {
            	return parseHereString();
            }
            case '|':
            {
                consume();
                return parseComment(true);
            }
            default:
                return parseDefaultToken(ch);
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
        else if (ch != EOR && !(SchemeScannerUtilities.isWhitespaceChar(ch))) {
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

    private int getPosition() {
		return mTokenEnd;
	}

	private boolean isEndPosition(int position) {
		return position >= mRangeEnd;
	}


    public String getText(int offset, int length) {
        try {
            return mDocument.get(offset, length);
        }
        catch (BadLocationException exception) {
            return "";
        }
    }

    public void setRange(IDocument document, int offset, int length) {
        mDocument = document;
        mRangeStart = offset;
        mRangeEnd = offset + length;
        mTokenStart = mTokenEnd = mRangeStart;
    }
}