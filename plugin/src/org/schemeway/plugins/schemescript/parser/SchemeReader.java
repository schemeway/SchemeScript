/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.parser;

import org.eclipse.jface.text.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SchemeReader {
	private SchemeScanner mScanner;
	private int mStart;
	private int mEnd;
	private SchemeToken mToken;
	private IDocument mDocument;

	private void initializeScanner(IDocument document, int start, int end) {
		mDocument = document;
		mScanner = new SchemeScanner();
		mStart = start;
		mEnd = start;
		mScanner.setRange(document, start, end - start);
		mToken = null;
	}

	public Region nextExpression(IDocument document, int start, int end) {

		initializeScanner(document, start, end);

		try {
			parseExpression();
		}
		catch (ParsingException exception) {
			mEnd = exception.getOffset();
		}

		return new Region(mStart, mEnd - mStart);
	}

	/**
	 * 
	 */
	private void parseExpression() throws ParsingException {
		boolean done = false;

		while (!done) {
			int type = lookahead().getType();
			switch (type) {
			case SchemeToken.COMMENT:
			case SchemeToken.WSPACE: {
				consume();
				break;
			}
			case SchemeToken.EXPR_COMMENT_PREFIX: {
				consume();
				parseExpression();
				break;
			}
			case SchemeToken.VECTORPREFIX:
			case SchemeToken.LPAREN: {
				if (type == SchemeToken.VECTORPREFIX)
					consume();
				consume();
				parseExpressionList();
				if (!(type == SchemeToken.RPAREN)) {
					int offset = lookahead() == SchemeToken.EOF ? mDocument.getLength() : lookahead().getOffset();
					throw new ParsingException("RPAREN expected", offset);
				}
				consume();
				done = true;
				break;
			}
			case SchemeToken.RPAREN: {
				throw new ParsingException("unexpected RPAREN", lookahead().getOffset());
			}
			case SchemeToken.EOFTOK:
				throw new ParsingException("End of buffer!", mDocument.getLength());
			default:
				consume();
				done = true;
			}
		}
		mEnd = mScanner.getTokenOffset();
	}

	private void parseExpressionList() throws ParsingException {
		skipWhitespaces();
		while (lookahead() != SchemeToken.EOF && lookahead().getType() != SchemeToken.RPAREN) {
			parseExpression();
			skipWhitespaces();
		}
	}

	private void skipWhitespaces() {
		int type = lookahead().getType();
		while (type == SchemeToken.COMMENT || type == SchemeToken.WSPACE) {
			consume();
			type = lookahead().getType();
		}
	}

	private SchemeToken lookahead() {
		if (mToken == null) {
			consume();
		}
		return mToken;
	}

	private void consume() {
		if (mToken != SchemeToken.EOF)
			mToken = mScanner.nextToken();
	}

	private class ParsingException extends Exception {
		/**
         * 
         */
        private static final long serialVersionUID = 1L;
        private int mOffset;

		public ParsingException(String message, int offset) {
			super(message);
			mOffset = offset;
		}

		public int getOffset() {
			return mOffset;
		}

	}
}
