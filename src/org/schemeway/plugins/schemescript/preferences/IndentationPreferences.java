/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;

import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.parser.*;

public class IndentationPreferences extends PreferencePage implements IWorkbenchPreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".indentation.";
    public final static String INDENT_SCHEMES = PREFIX + "schemes";
    public final static String TAB_WIDTH = PREFIX + "tabwidth";

    public final static String ROW_SYMBOL = "Symbol";
    public final static String ROW_SCHEME = "Indentation scheme";
    public final static String ROW_HINT = "Hint";

    public final static String[] ROW_NAMES = new String[] {
                ROW_SYMBOL, ROW_SCHEME, ROW_HINT
    };

    private TableViewer mViewer;
    private IndentationSchemeList mSchemeList;

    private final static IndentationScheme[] INDENT_DEFAULTS = new IndentationScheme[] {
                new IndentationScheme("define", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-syntax", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-macro", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-simple-class", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-class", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-procedure", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-record", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("define-record-type", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("lambda", IndentationScheme.DEFINITION, 0),
                new IndentationScheme("cond", IndentationScheme.DEFAULT, 0),
                new IndentationScheme("if", IndentationScheme.IF, 0),
                new IndentationScheme("begin", IndentationScheme.SEQUENCE, 0),
                new IndentationScheme("case", IndentationScheme.WITH, 1),
                new IndentationScheme("when", IndentationScheme.WITH, 1),
                new IndentationScheme("unless", IndentationScheme.WITH, 1),
                new IndentationScheme("let", IndentationScheme.WITH, 1),
                new IndentationScheme("let*", IndentationScheme.WITH, 1),
                new IndentationScheme("letrec", IndentationScheme.WITH, 1),
                new IndentationScheme("let-values", IndentationScheme.WITH, 1),
                new IndentationScheme("let*-values", IndentationScheme.WITH, 1),
                new IndentationScheme("syntax-rules", IndentationScheme.WITH, 1),
                new IndentationScheme("with-input-from-file", IndentationScheme.WITH, 1),
                new IndentationScheme("with-output-to-file", IndentationScheme.WITH, 1),
                new IndentationScheme("call-with-output-file", IndentationScheme.WITH, 1),
                new IndentationScheme("call-with-input-file", IndentationScheme.WITH, 1),
                new IndentationScheme("make", IndentationScheme.WITH, 1),
                new IndentationScheme("object", IndentationScheme.WITH, 1),
                new IndentationScheme("try-catch", IndentationScheme.WITH, 1),
                new IndentationScheme("try-finally", IndentationScheme.WITH, 1),
                new IndentationScheme("receive", IndentationScheme.WITH, 2),
                new IndentationScheme("catch", IndentationScheme.WITH, 2)
    };

    public void init(IWorkbench workbench) {
    }

    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        composite.setLayout(layout);

        createAllControls(composite);

        initializeProvider();
        return composite;
    }

    private void createAllControls(Composite composite) {
        Label label = new Label(composite, SWT.NONE);
        label.setText("Configure the symbol categories for proper Scheme indentation.");
        GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        data.horizontalSpan = 3;
        label.setLayoutData(data);

        final Button deleteButton = new Button(composite, SWT.NONE);
        deleteButton.setText("Delete");
        deleteButton.setToolTipText("Delete the selected symbols");
        deleteButton.setEnabled(false);

        final Button addButton = new Button(composite, SWT.NONE);
        addButton.setText("Add");
        addButton.setToolTipText("Add a new symbol");
        addButton.setEnabled(false);

        final Text text = new Text(composite, SWT.SINGLE | SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        text.setLayoutData(data);

        Table table = new Table(composite, SWT.BORDER | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL);
        TableColumn column0 = new TableColumn(table, SWT.LEFT);
        TableColumn column1 = new TableColumn(table, SWT.LEFT);
        TableColumn column2 = new TableColumn(table, SWT.LEFT);
        column0.setText(ROW_SYMBOL);
        column0.setWidth(150);
        column0.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                mViewer.setSorter(new IndentationSchemeSorter(ROW_SYMBOL));
            }
        });
        column1.setText(ROW_SCHEME);
        column1.setWidth(150);
        column1.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                mViewer.setSorter(new IndentationSchemeSorter(ROW_SCHEME));
            }
        });
        column2.setText(ROW_HINT);
        column2.setWidth(60);

        table.setLinesVisible(true);
        table.setHeaderVisible(true);

        createTableViewer(table);

        addListeners(table, addButton, deleteButton, text);
    }
    

    private void createTableViewer(Table table) {
        GridData data;
        mViewer = new TableViewer(table);
        data = new GridData(GridData.FILL_BOTH);
        data.horizontalSpan = 3;
        table.setLayoutData(data);

        mViewer.setColumnProperties(ROW_NAMES);
        mViewer.setUseHashlookup(true);

        TextCellEditor symbolEditor = new TextCellEditor(table);
        ComboBoxCellEditor schemeEditor = new ComboBoxCellEditor(table, IndentationScheme.ALL_SCHEMES, SWT.READ_ONLY);
        TextCellEditor hintEditor = new TextCellEditor(table);
        ((Text) hintEditor.getControl()).addVerifyListener(new VerifyListener() {
            public void verifyText(VerifyEvent e) {
                e.doit = "0123456789".indexOf(e.text) >= 0;
            }
        });

        mViewer.setSorter(new IndentationSchemeSorter(ROW_SYMBOL));
        mViewer.setCellEditors(new CellEditor[] {
                                symbolEditor, schemeEditor, hintEditor
        });
        mViewer.setLabelProvider(new IndentationSchemeLabelProvider());
        mViewer.setCellModifier(new IndentationSchemeCellModifier(mViewer));
    }

    private void addListeners(final Table table, final Button addButton, final Button deleteButton, final Text text) {
        table.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteButton.setEnabled(table.getSelectionCount() > 0);
            }
        });

        text.addKeyListener(new KeyAdapter() {
            public void keyReleased(KeyEvent event) {
                String symbol = text.getText();
                addButton.setEnabled(SchemeScannerUtilities.isIdentifier(symbol));
            }
        });

        addButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                mSchemeList.addScheme(new IndentationScheme(text.getText(), IndentationScheme.DEFAULT, 0));
                text.setText("");
                addButton.setEnabled(false);
            }
        });

        deleteButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                Object data = table.getItem(table.getSelectionIndex()).getData();
                mSchemeList.removeScheme((IndentationScheme) data);
                deleteButton.setEnabled(false);
            }
        });
    }

    public boolean performOk() {
        storeValues();
        return true;
    }

    public void performDefaults() {
        super.performDefaults();

        mSchemeList = new IndentationSchemeList(INDENT_DEFAULTS);
        mViewer.setInput(mSchemeList);
        storeValues();
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return SchemeScriptPlugin.getDefault().getPreferenceStore();
    }

    public static void initializeDefaults(IPreferenceStore store) {
        PreferenceUtil.setDefaultIndentationSchemes(store, INDENT_SCHEMES, INDENT_DEFAULTS);
    }

    private void initializeProvider() {
        IPreferenceStore store = getPreferenceStore();
        IndentationScheme[] schemes = PreferenceUtil.getIndentationSchemes(store, INDENT_SCHEMES);
        mSchemeList = new IndentationSchemeList(schemes);

        mViewer.setContentProvider(new IndentationSchemeContentProvider(mViewer));
        mViewer.setInput(mSchemeList);
    }

    private void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        PreferenceUtil.setIndentationSchemes(store, INDENT_SCHEMES, mSchemeList.getSchemes());
    }
}