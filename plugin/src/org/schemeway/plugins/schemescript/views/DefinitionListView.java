/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.views;

import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;

public class DefinitionListView extends ViewPart {

    public final static String DEFINITION_LIST_ID = SchemeScriptPlugin.PLUGIN_NS + ".views.definitionList";

    private static class SymbolLabelProvider extends LabelProvider implements ITableLabelProvider {
        public String getColumnText(Object obj, int index) {
            IFile file = ((SymbolEntry) obj).getFile();
			switch (index) {
                case 0:
                    return "";
                case 1:
                    return ((SymbolEntry) obj).getDescription();
                case 2:
                    return file == null ? "" : file.getName();
                case 3:
                    return file == null ? "" : file.getParent().getRawLocation().toOSString();
                default:
                    return "";
            }
        }

        public Image getColumnImage(Object obj, int index) {
            return null;
        }
    }

    private SymbolEntry[] mShownEntries = new SymbolEntry[0];
    private TableViewer mViewer;

    public DefinitionListView() {
        super();
    }

    public void createPartControl(Composite parent) {
        mViewer = new TableViewer(parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE | SWT.FULL_SELECTION);
        final Table table = mViewer.getTable();

        TableColumn typeColumn = new TableColumn(table, SWT.LEFT);
        typeColumn.setText("");
        typeColumn.setWidth(18);

        TableColumn categoryColumn = new TableColumn(table, SWT.LEFT);
        categoryColumn.setText("Description");
        categoryColumn.setWidth(200);
        
        TableColumn fileColumn = new TableColumn(table, SWT.LEFT);
        fileColumn.setText("File");
        fileColumn.setWidth(150);

        TableColumn locationColumn = new TableColumn(table, SWT.LEFT);
        locationColumn.setText("Location");
        locationColumn.setWidth(450);

        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        mViewer.setContentProvider(new ArrayContentProvider());
        mViewer.setLabelProvider(new SymbolLabelProvider());
        mViewer.setInput(mShownEntries);

        mViewer.addDoubleClickListener(new IDoubleClickListener() {
            public void doubleClick(DoubleClickEvent event) {
                IStructuredSelection selection = (IStructuredSelection) mViewer.getSelection();
                if (!selection.isEmpty()) {
                    openEditorAtLine((SymbolEntry) selection.getFirstElement());
                }
            }
        });
    }

    public void setFocus() {
        mViewer.getTable().setFocus();
    }

    public void setEntries(SymbolEntry[] entries) {
        mShownEntries = entries;
        updateTable();
    }

    private void updateTable() {
        mViewer.getTable().setRedraw(false);
        mViewer.setInput(mShownEntries);
        if (mShownEntries.length > 0) {
            mViewer.setSelection(new StructuredSelection(mShownEntries[0]), true);
        }
        mViewer.getTable().setRedraw(true);
    }

    public static void openEditorAtLine(SymbolEntry entry) {
        if (entry == null)
            return;
        
        if (entry.getFile() == null)
        	return ;
        
        SchemeScriptTools.openEditor(entry.getFile(), entry.getOffset(), entry.getLength());
    }
    
    public static void showInView(SymbolEntry[] entries) {
        try {
        DefinitionListView view = (DefinitionListView) PlatformUI.getWorkbench()
                                                                 .getActiveWorkbenchWindow()
                                                                 .getActivePage()
                                                                 .showView(DefinitionListView.DEFINITION_LIST_ID);
        view.setEntries(entries);
        view.setFocus();
        }
        catch (PartInitException exception) {
            SchemeScriptPlugin.logException("Unable to open view", exception);
        }
    }


}