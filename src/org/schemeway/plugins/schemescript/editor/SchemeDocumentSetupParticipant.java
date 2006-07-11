/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.filebuffers.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.*;

public class SchemeDocumentSetupParticipant implements IDocumentSetupParticipant {

	public static final String SCHEME_PARTITIONING = "___scheme_partitioning_";
	
    public SchemeDocumentSetupParticipant() {
    }

    public void setup(IDocument document) {
        String[] partitions = new String[]
        {
            SchemePartitionScanner.SCHEME_STRING, SchemePartitionScanner.SCHEME_COMMENT, SchemePartitionScanner.SCHEME_HERESTRING
        };

        IDocumentPartitioner partitioner = new FastPartitioner(new SchemePartitionScanner(), partitions);
        IDocumentExtension3 documentExtension3 = (IDocumentExtension3) document;
        documentExtension3.setDocumentPartitioner(SCHEME_PARTITIONING, partitioner);
        partitioner.connect(document);
    }
}