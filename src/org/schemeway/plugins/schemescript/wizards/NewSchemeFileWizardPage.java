/*
 * Copyright (c) 2002-2003 Nu Echo Inc. All rights reserved.
 */
package org.schemeway.plugins.schemescript.wizards;

import java.io.*;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.*;
import org.schemeway.plugins.schemescript.tools.*;

/**
 * @author Nu Echo Inc.
 */
public class NewSchemeFileWizardPage extends WizardNewFileCreationPage
{
    Text mModuleName;
    Text mDescription;

    public NewSchemeFileWizardPage(String title, IStructuredSelection selection)
    {
        super(title, selection);
        setDescription("Create a new Kawa Scheme file");
        setTitle("Create Kawa Scheme File");
    }

    public void createControl(Composite parent)
    {
        super.createControl(parent);
        Composite composite = (Composite) getControl();

        Group group = new Group(composite, SWT.NONE);
        group.setText("File information");
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        GridData data = new GridData(GridData.FILL_BOTH);
        group.setLayout(layout);
        group.setLayoutData(data);

        Label label = new Label(group, SWT.NONE);
        label.setText("Module Description: ");
        label.setEnabled(true);

        mDescription = new Text(group, SWT.SINGLE | SWT.BORDER);
        mDescription.setText("");
        mDescription.setEnabled(true);

        data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        mDescription.setLayoutData(data);

        label = new Label(group, SWT.NONE);
        label.setText("Module fully-qualified name: ");
        label.setEnabled(true);

        mModuleName = new Text(group, SWT.SINGLE | SWT.BORDER);
        mModuleName.setText("");
        mModuleName.setEnabled(true);

        data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        mModuleName.setLayoutData(data);

        setControl(composite);
    }

    public InputStream getInitialContents()
    {
        StringBuffer buffer = new StringBuffer();
        buffer.append(Comments.createHeaderComment("\n", mDescription.getText()));
        buffer.append(Comments.createSectionComment("\n", "Module declaration"));
        buffer.append("(module-name <").append(mModuleName.getText()).append(">)\n\n\n");
        buffer.append(Comments.createChapterComment("\n", "Public interface"));
        buffer.append(Comments.createChapterComment("\n", "Private interface"));
        return new ByteArrayInputStream(buffer.toString().getBytes());
    }
}
