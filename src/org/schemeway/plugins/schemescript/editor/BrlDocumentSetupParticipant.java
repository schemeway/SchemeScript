/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.filebuffers.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.*;

/**
 * @author SchemeWay Project.
 *
 */
public class BrlDocumentSetupParticipant implements IDocumentSetupParticipant {

	public BrlDocumentSetupParticipant() {
	}
	
    public void setup(IDocument document) {
        String[] partitions = new String[]
        {
            SchemePartitionScanner.SCHEME_STRING, SchemePartitionScanner.SCHEME_COMMENT, SchemePartitionScanner.SCHEME_HERESTRING
        };

        IDocumentPartitioner partitioner = new FastPartitioner(new BrlPartitionScanner(), partitions);
        IDocumentExtension3 documentExtension3 = (IDocumentExtension3) document;
        documentExtension3.setDocumentPartitioner(SchemeDocumentSetupParticipant.SCHEME_PARTITIONING, partitioner);
        partitioner.connect(document);
   }
}
