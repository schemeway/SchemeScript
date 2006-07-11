/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;

public class ForwardTokenIterator implements ISchemeTokenIterator {
    private IDocument mDocument;
    private SchemeScanner mScanner;
    private ITypedRegion mCurrentPartition;
    private int mPosition;
    private int mEnd;

    public ForwardTokenIterator(IDocument document) {
        mDocument = document;
        mScanner = new SchemeScanner();
    }

    public SchemeToken nextToken() {
        return nextToken(true);
    }
    
    public SchemeToken nextToken(boolean ignoreWS) {
        SchemeToken result = null;

        while (result == null) {
            if (mPosition == mEnd)
                fetchNextPartition();

            if (mCurrentPartition == null)
                result = SchemeToken.EOF;
            else {
                if (SchemePartitionScanner.isStringPartition(mCurrentPartition.getType())) {
                    int length = mEnd - mPosition;
                    int start = mPosition;
                    mPosition = mEnd;
                    return SchemeToken.createString(start, length);
                }
                else
                    if (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                        fetchNextPartition();
                        continue;
                    }
                    else {
                        SchemeToken token = mScanner.nextToken();
                        mPosition = mPosition + token.getLength();
                        if (!ignoreWS || token.getType() != SchemeToken.WSPACE) {
                            result = token;
                        }
                    }
            }
        }
        return result;
    }

    public void setPosition(int position) {
        if (position < mDocument.getLength()) {
		    mCurrentPartition = SchemeTextUtilities.getPartition(mDocument, position);
		    if (SchemePartitionScanner.isStringPartition(mCurrentPartition.getType())) {
		        mPosition = mCurrentPartition.getOffset();
		        mEnd = mPosition + mCurrentPartition.getLength();
		    }
		    else
		        if (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
		            mPosition = mCurrentPartition.getOffset();
		            mEnd = mPosition + mCurrentPartition.getLength();
		        }
		        else {
		            mPosition = position;
		            mEnd = mCurrentPartition.getOffset() + mCurrentPartition.getLength();
		            mScanner.setRange(mDocument,
		                              position,
		                              (mCurrentPartition.getOffset() + mCurrentPartition.getLength()) - mPosition);
		        }
		}
		else {
		    mPosition = mEnd = position;
		    mCurrentPartition = null;
		}
    }

	private ITypedRegion getCurrentPartition() {
		return SchemeTextUtilities.getPartition(mDocument, mPosition);
	}

    private void fetchNextPartition() {
        // skip over comment partitions...
		if (mPosition < mDocument.getLength()) {
		    mCurrentPartition = getCurrentPartition();
		    if (mCurrentPartition == null)
		    	return;
		    
		    while (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
		        mPosition = mCurrentPartition.getOffset() + mCurrentPartition.getLength();
		        ITypedRegion partition = getCurrentPartition();
		        if (partition == null)
		        	break;
		        
		        mCurrentPartition = partition;
		    }
		    mPosition = mCurrentPartition.getOffset();
		    mEnd = mPosition + mCurrentPartition.getLength();
		    if (!SchemePartitionScanner.isStringPartition(mCurrentPartition.getType()))
		        mScanner.setRange(mDocument, mPosition, mEnd - mPosition);
		}
		else
		    mCurrentPartition = null;
    }
}