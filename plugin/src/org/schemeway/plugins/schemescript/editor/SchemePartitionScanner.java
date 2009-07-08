/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.parser.*;

public class SchemePartitionScanner implements IPartitionTokenScanner {
	private final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".partitions.";
	public final static String SCHEME_DEFAULT = PREFIX + "default";
	public final static String SCHEME_COMMENT = PREFIX + "comment";
	public final static String SCHEME_STRING = PREFIX + "tag";
	public final static String SCHEME_HERESTRING = PREFIX + "herestring";

	protected static final Token TOKEN_COMMENT = new Token(SCHEME_COMMENT);
	protected static final Token TOKEN_STRING = new Token(SCHEME_STRING);
	protected static final Token TOKEN_HERESTRING = new Token(SCHEME_HERESTRING);
	protected static final Token TOKEN_DEFAULT = new Token(IDocument.DEFAULT_CONTENT_TYPE);

	private static final int STATE_DEFAULT = 0;
	private static final int STATE_VBAR = 1;
	private static final int STATE_CHARACTER = 2;
	private static final int STATE_SHARP = 3;
	private static final int STATE_ESCAPE = 4;
	private static final int STATE_SHARP_LT = 5;
	private static final int STATE_DONE = 6;

	private IDocument mDocument;
	private int mEnd;
	private int mTokenStart;
	private int mPosition;
	private int mState;

	public SchemePartitionScanner() {

	}

	public void setPartialRange(IDocument document, int offset, int length, String contentType, int partitionOffset) {
		if (partitionOffset >= 0 && contentType != null) {
			setRange(document, partitionOffset, offset - partitionOffset + length);
		}
		else {
			setRange(document, offset, length);
		}
	}

	public int getTokenLength() {
		return mPosition - mTokenStart;
	}

	public int getTokenOffset() {
		return mTokenStart;
	}

	public IToken nextToken() {
		if (mPosition >= mEnd) {
			return Token.EOF;
		}

		mTokenStart = mPosition;
		return scanToken();
	}

	protected Token scanToken() {
		Token result;
		switch (lookahead()) {
		case '#': {
			consume();
			if (lookahead() == '|') {
				consume();
				result = scanMultilineComment();
			}
			else if (lookahead() == '<') {
				result = scanHereString();
			}
			else if (lookahead() == ';') {
				result = scanExpression();
			}
			else if (lookahead() == '!' && currentLine() == 0)
			{
				result = scanSinglelineComment();
			}
			else {
				result = scanDefault();
			}
			break;
		}
		case '"': {
			consume();
			result = scanString();
			break;
		}
		case ';': {
			consume();
			result = scanSinglelineComment();
			break;
		}
		default: {
			result = scanDefault();
		}
		}
		return result;
	}

	private int currentLine() {
		try {
			return mDocument.getLineOfOffset(mPosition);
		}
		catch (BadLocationException e) {
			return -1;
		}
	}

	private Token scanExpression() {
		consume();
		SchemeReader reader = new SchemeReader();
		Region region = reader.nextExpression(mDocument, mPosition, mDocument.getLength());
		
		mPosition = region.getOffset() + region.getLength();

		return TOKEN_COMMENT;
	}

	private Token scanHereString() {
		consume();
		if (lookahead() == '<') {
			consume();
			String tag = readToEndOfLine();
			String nextLine = readToEndOfLine();
			while (nextLine != null && !nextLine.equals(tag)) {
				nextLine = readToEndOfLine();
			}

			return TOKEN_HERESTRING;
		}
		else {
			return scanDefault();
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
			mPosition = startPosition + length;
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

	private Token scanMultilineComment() {
		// we assume that the starting #| has already been read.
		mState = STATE_DEFAULT;
		char ch;
		while (mPosition < mEnd) {
			ch = lookahead();
			if (ch == '#') {
				if (mState == STATE_VBAR) {
					mState = STATE_DONE;
				}
				else {
					mState = STATE_DEFAULT;
				}
			}
			else if (ch == '|') {
				mState = STATE_VBAR;
			}
			else {
				mState = STATE_DEFAULT;
			}
			consume();

			if (mState == STATE_DONE)
				break;

		}
		return TOKEN_COMMENT;
	}

	private Token scanSinglelineComment() {
		char ch = lookahead();
		while (mPosition < mEnd && ch != '\r' && ch != '\n') {
			consume();
			ch = lookahead();
		}
		// Ensure that the end-of-line is also part of the partition
		// This simplifies a lot of partition-related processing.
		if (mPosition < mEnd && ch == '\r') {
			consume();
		}
		if (mPosition < mEnd && ch == '\n') {
			consume();
		}
		return TOKEN_COMMENT;
	}

	private Token scanString() {
		char ch;
		mState = STATE_DEFAULT;
		while (mPosition < mEnd) {
			ch = lookahead();
			if (ch == '"') {
				if (mState == STATE_ESCAPE) {
					mState = STATE_DEFAULT;
				}
				else {
					mState = STATE_DONE;
				}
			}
			else if (ch == '\\') {
				if (mState == STATE_DEFAULT) {
					mState = STATE_ESCAPE;
				}
				else {
					mState = STATE_DEFAULT;
				}
			}
			else {
				mState = STATE_DEFAULT;
			}
			consume();
			if (mState == STATE_DONE)
				break;
		}
		return TOKEN_STRING;
	}

	private Token scanDefault() {
		char ch;
		mState = STATE_DEFAULT;
		while (mPosition < mEnd) {
			ch = lookahead();
			switch (ch) {
			case ';': {
				if (mState == STATE_DEFAULT) {
					mState = STATE_DONE;
				}
				else if (mState == STATE_SHARP) {
					mPosition--;
					mState = STATE_DONE;
				}
				else {
					consume();
				}
				break;
			}
			case '"': {
				if (mState == STATE_DEFAULT || mState == STATE_SHARP) {
					mState = STATE_DONE;
				}
				else {
					mState = STATE_DEFAULT;
					consume();
				}
				break;
			}
			case '#': {
				mState = STATE_SHARP;
				consume();
				break;
			}
			case '\\': {
				if (mState == STATE_SHARP) {
					mState = STATE_CHARACTER;
				}
				else {
					mState = STATE_DEFAULT;
				}
				consume();
				break;
			}
			case '|': {
				if (mState == STATE_SHARP) {
					mState = STATE_DONE;
					mPosition--;
				}
				else {
					mState = STATE_DEFAULT;
					consume();
				}

				break;
			}
			case '<': {
				if (mState == STATE_SHARP) {
					mState = STATE_SHARP_LT;
					consume();
				}
				else if (mState == STATE_SHARP_LT) {
					mPosition -= 2;
					mState = STATE_DONE;
				}
				else {
					mState = STATE_DEFAULT;
					consume();
				}
				break;
			}
			default: {
				if (endOfDefaultPartition()) {
					mState = STATE_DONE;
					break;
				}
				else {
					mState = STATE_DEFAULT;
					consume();
				}
			}
			}
			if (mState == STATE_DONE)
				break;
		}
		return TOKEN_DEFAULT;
	}

	protected boolean endOfDefaultPartition() {
		return false;
	}

	public void setRange(IDocument document, int offset, int length) {
		mDocument = document;
		mEnd = offset + length;
		mPosition = offset;
	}

	protected final char lookahead() {
		try {
			return mDocument.getChar(mPosition);
		}
		catch (BadLocationException exception) {
			// should NOT happen!
			return '\0';
		}
	}

	protected final int getPosition() {
		return mPosition;
	}

	protected final boolean isEndPosition(int position) {
		return position >= mEnd;
	}

	protected final void consume() {
		if (mPosition < mEnd) {
			mPosition++;
		}
	}

	public static boolean isStringPartition(String partitionName) {
		return partitionName == SCHEME_STRING || partitionName == SCHEME_HERESTRING;
	}
}