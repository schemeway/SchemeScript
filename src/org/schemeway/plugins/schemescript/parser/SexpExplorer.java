/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.*;

/**
 * This class encapsulates all the methods used to navigate S-expressions.
 */
public class SexpExplorer {
    public final static int TYPE_ERROR = 0;
    public final static int TYPE_SYMBOL = TYPE_ERROR + 1;
    public final static int TYPE_LIST = TYPE_ERROR + 2;
    public final static int TYPE_STRING = TYPE_ERROR + 3;
    public final static int TYPE_CONSTANT = TYPE_ERROR + 4;
    public final static int TYPE_OTHER = TYPE_ERROR + 5;
    public final static int TYPE_NONE = TYPE_ERROR + 6;

    private final static int DIRECTION_FORWARD = 1;
    private final static int DIRECTION_BACKWARD = 2;

    private IDocument mDocument;
    private int mSexpStart;
    private int mListStart;
    private int mSexpEnd;
    private int mSexpType;
    private int mCurrentDirection;

    private ISchemeTokenIterator mForwardIterator;
    private ISchemeTokenIterator mBackwardIterator;
    private ISchemeTokenIterator mTokenIterator;

    // TODO - update the tokens incrementally when changes are done to the
    // document. This way, we could keep only one SexpExplorer for each
    // SchemeEditor and the tokenization would not be done over and over again

    public SexpExplorer(IDocument document) {
        Assert.isNotNull(document);
        mDocument = document;
        mSexpType = TYPE_ERROR;
        mSexpStart = 0;
        mSexpEnd = 0;
        mForwardIterator = createForwardTokenIterator();
        mBackwardIterator = createBackwardTokenIterator();
    }

    public void dispose() {
        mDocument = null;
        mForwardIterator = null;
        mBackwardIterator = null;
    }

    public IDocument getDocument() {
        return mDocument;
    }

    public int getSexpEnd() {
        return mSexpEnd;
    }

    public int getSexpStart() {
        return mSexpStart;
    }

    public int getListStart() {
        return mListStart;
    }

    public int getSexpType() {
        return mSexpType;
    }

    public String getText() {
        if (mSexpType != TYPE_ERROR && mSexpStart != mSexpEnd) {
            try {
                return mDocument.get(mSexpStart, mSexpEnd - mSexpStart);
            }
            catch (BadLocationException exception) {
                return "";
            }
        }
        else
            return "";
    }

    /**
     * Call this method to jump over the next S-expression
     */
    public boolean forwardSexpression(int start) {
        return scanSexpression(DIRECTION_FORWARD, SchemeToken.LPAREN, SchemeToken.RPAREN, start, 0);
    }

    public boolean backwardSexpression(int start) {
        return scanSexpression(DIRECTION_BACKWARD, SchemeToken.RPAREN, SchemeToken.LPAREN, start, 0);
    }

    public boolean upSexpression(int start) {
        return scanSexpression(DIRECTION_BACKWARD, SchemeToken.RPAREN, SchemeToken.LPAREN, start, -1);
    }

    public boolean downSexpression(int start) {
        setupIterator(DIRECTION_FORWARD, start);

        while (true) {
            SchemeToken currentToken = mTokenIterator.nextToken();
            int type = currentToken.getType();
            if (type == SchemeToken.EOFTOK || type == SchemeToken.RPAREN)
                return false;
            if (type == SchemeToken.LPAREN) {
                mSexpStart = currentToken.getOffset() + currentToken.getLength();
                mSexpEnd = mSexpStart;
                return true;
            }
        }
//        SchemeToken currentToken = mTokenIterator.nextToken();
//        int type = currentToken.getType();
//        if (type != SchemeToken.LPAREN)
//            return false;
//        else {
//            mSexpStart = currentToken.getOffset() + currentToken.getLength();
//            mSexpEnd = mSexpStart;
//            return true;
//        }
    }

    private final boolean scanSexpression(int direction, int enteringType, int exitingType, int start, int endLevel) {
        setupIterator(direction, start);
        mSexpType = TYPE_ERROR;

        SchemeToken currentToken = mTokenIterator.nextToken();
        int type = currentToken.getType();
        if ((currentToken == SchemeToken.EOF))
            return false;
        else {
            if (direction == DIRECTION_FORWARD && syntacticPrefixType(type)) {
                mSexpStart = currentToken.getOffset();
                mSexpEnd = mSexpStart + currentToken.getLength();
                currentToken = mTokenIterator.nextToken(false);
                type = currentToken.getType();
                if (type == SchemeToken.WSPACE)
                    return true;
            }

            if (type == exitingType) {
                if (endLevel < 0) {
                    mSexpStart = currentToken.getOffset();
                    mSexpEnd = currentToken.getOffset() + currentToken.getLength();
                    return true;
                }
                else
                    return false;
            }
            else
                if (type == enteringType || endLevel < 0) {
                    if (direction == DIRECTION_FORWARD)
                        mSexpStart = currentToken.getOffset();
                    else
                        mSexpEnd = currentToken.getOffset() + currentToken.getLength();

                    int level = (type == enteringType ? 1 : 0);
                    boolean done = false;
                    do {
                        currentToken = mTokenIterator.nextToken();
                        type = currentToken.getType();
                        if (currentToken == SchemeToken.EOF)
                            done = true;
                        else {
                            if (type == enteringType)
                                level++;
                            else
                                if (type == exitingType)
                                    level--;
                            done = (level == endLevel);
                        }
                    }
                    while (!done);

                    if (type == exitingType) {
                        mSexpType = TYPE_LIST;
                        if (direction == DIRECTION_FORWARD)
                            mSexpEnd = currentToken.getOffset() + currentToken.getLength();
                        else {
                            mListStart = mSexpStart = currentToken.getOffset();
                            currentToken = mTokenIterator.nextToken(false);
                            type = currentToken.getType();
                            if (syntacticPrefixType(type))
                                mSexpStart -= currentToken.getLength();
                        }
                        return true;
                    }
                    else
                        return false;
                }
                else {
                    mListStart = mSexpStart = currentToken.getOffset();
                    mSexpEnd = mSexpStart + currentToken.getLength();
                    mSexpType = translateType(currentToken.getType());
                    return true;
                }
        }
    }

    /* ---------- Helpers ----------- */
    private static final int translateType(int tokenType) {
        switch (tokenType) {
            case SchemeToken.CONSTANT:
                return TYPE_CONSTANT;
            case SchemeToken.SYMBOL:
                return TYPE_SYMBOL;
            case SchemeToken.STRING:
                return TYPE_STRING;
            default:
                return TYPE_OTHER;
        }
    }

    private static final boolean syntacticPrefixType(int type) {
        return type == SchemeToken.VECTORPREFIX
               || type == SchemeToken.QUOTE
               || type == SchemeToken.UNQUOTE
               || type == SchemeToken.BACKQUOTE
               || type == SchemeToken.UNQUOTE_SPLICING;
    }

    private final void setupIterator(int direction, int start) {
        if (direction != mCurrentDirection) {
            mCurrentDirection = direction;
            if (direction == DIRECTION_BACKWARD)
                mTokenIterator = mBackwardIterator;
            else
                mTokenIterator = mForwardIterator;

        }
        mTokenIterator.setPosition(start);
    }

    protected final ISchemeTokenIterator createForwardTokenIterator() {
        return new ForwardTokenIterator(mDocument);
    }

    protected final ISchemeTokenIterator createBackwardTokenIterator() {
        return new BackwardTokenIterator(mDocument);
    }
}