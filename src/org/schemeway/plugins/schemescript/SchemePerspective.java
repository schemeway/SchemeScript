/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript;

import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.views.*;

public class SchemePerspective implements IPerspectiveFactory {

    public SchemePerspective() {
        super();
    }

    public void createInitialLayout(IPageLayout layout) {
        String editorArea = layout.getEditorArea();
        
        layout.addView(IPageLayout.ID_OUTLINE, IPageLayout.LEFT, 0.20f, editorArea);
        
        layout.addFastView(DefinitionListView.DEFINITION_LIST_ID);
    }
}
