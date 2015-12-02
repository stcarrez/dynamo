<?xml version="1.0" encoding="UTF-8"?>
<!--  extract.xsl - Extract information from plugin XML configuration  -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" indent="yes"/>


    <xsl:template match="/module">
      <xsl:if test="count(managed-bean) &gt; 0">
        <table width="100%">
          <tr>
            <th width="30%" align="left">Name</th>
            <th width="70%" align="left">Description</th>
          </tr>
          <xsl:apply-templates select="managed-bean"/>          
        </table>
      </xsl:if>

      <xsl:if test="count(entity-permission) &gt; 0">
        <h3>Permissions</h3>
        <table width="100%">
          <tr>
            <th width="20%" align="left">Name</th>
            <th width="10%" align="left">Name</th>
            <th width="70%" align="left">Description</th>
          </tr>
          <xsl:apply-templates select="entity-permission"/>          
        </table>
      </xsl:if>

      <xsl:if test="count(context-param) &gt; 0">
        <h3>Configuration</h3>
        <table width="100%">
          <tr>
            <th width="30%" align="left">Name</th>
            <th width="70%" align="left">Description</th>
          </tr>
          <xsl:apply-templates select="context-param"/>
        </table>
      </xsl:if>

    </xsl:template>

    <xsl:template match="/hibernate-mapping">

      <xsl:if test="count(class) &gt; 0">
        <xsl:apply-templates match="class"/>
      </xsl:if>

    </xsl:template>

    <xsl:template match="managed-bean">
      <tr>
        <td><xsl:value-of select="managed-bean-name"/></td>
        <td><xsl:value-of select="description"/></td>
      </tr>
    </xsl:template>

    <xsl:template match="context-param">
      <tr>
        <td><xsl:value-of select="param-name"/></td>
        <td><xsl:value-of select="description"/></td>
      </tr>
      <tr>
        <td></td>
        <td><tt><xsl:value-of select="param-value"/></tt></td>
      </tr>
    </xsl:template>

    <xsl:template match="entity-permission">
      <tr>
        <td><xsl:value-of select="name"/></td>
        <td><xsl:value-of select="entity-type"/></td>
        <td><xsl:value-of select="description"/></td>
      </tr>
    </xsl:template>

    <xsl:template match="class">
      <h4><xsl:value-of select="@name"/></h4>
      <p><xsl:value-of select="comment"/></p>
      <table width="100%">
        <tr>
          <th width="15%" align="left">Type</th>
          <th width="15%" align="left">Ada</th>
          <th width="20%" align="left">Name</th>
          <th width="50%" align="left">Description</th>
        </tr>
        <xsl:apply-templates select="id"/>
        <xsl:apply-templates select="version"/>
        <xsl:apply-templates select="property"/>
        <xsl:apply-templates select="many-to-one"/>
      </table>
      <xsl:text>

      </xsl:text>
    </xsl:template>

    <xsl:template match="property">
      <tr>
        <td><xsl:value-of select="column/@sql-type"/></td>
        <td><xsl:value-of select="@type"/></td>
        <td><xsl:value-of select="@name"/><xsl:value-of select="column/@name"/></td>
        <td><xsl:value-of select="comment"/></td>
      </tr>
    </xsl:template>

    <xsl:template match="id">
      <tr>
        <td><xsl:value-of select="column/@sql-type"/></td>
        <td><xsl:value-of select="@type"/></td>
        <td><xsl:value-of select="column/@name"/></td>
        <td><xsl:value-of select="comment"/></td>
      </tr>
    </xsl:template>

    <xsl:template match="version">
      <tr>
        <td><xsl:value-of select="@type"/></td>
        <td></td>
        <td><xsl:value-of select="@column"/></td>
        <td><xsl:value-of select="comment"/></td>
      </tr>
    </xsl:template>

    <xsl:template match="many-to-one">
      <tr>
        <td><xsl:value-of select="column/@sql-type"/></td>
        <td><xsl:value-of select="@class"/></td>
        <td><xsl:value-of select="column/@name"/></td>
        <td><xsl:value-of select="comment"/></td>
      </tr>
    </xsl:template>

    <xsl:template match="text()|@*"/>

</xsl:stylesheet>