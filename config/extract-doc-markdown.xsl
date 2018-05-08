<?xml version="1.0" encoding="UTF-8"?>
<!--  extract.xsl - Extract information from plugin XML configuration  -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text" indent="no"/>

    <xsl:strip-space elements="context-param description comments country company price year" />

    <xsl:template match="/module">
      <xsl:if test="count(managed-bean) &gt; 0">
| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
<xsl:apply-templates select="managed-bean"/>          
      </xsl:if>

      <xsl:if test="count(entity-permission) &gt; 0">
### Permissions
| Name           | Entity type  | Description                                                |
|:---------------|:-------------|:-----------------------------------------------------------|
          <xsl:apply-templates select="entity-permission"/>          
      </xsl:if>

      <xsl:if test="count(context-param) &gt; 0">
### Configuration
| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
<xsl:apply-templates select="context-param"/>
      </xsl:if>

    </xsl:template>

    <xsl:template match="/hibernate-mapping">

      <xsl:if test="count(class) &gt; 0">
        <xsl:apply-templates match="class"/>
      </xsl:if>

    </xsl:template>

    <xsl:template match="/query-mapping">

      <xsl:if test="count(query) &gt; 0">
### Queries
| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
<xsl:for-each select="query">|<xsl:value-of select="@name"/>|<xsl:value-of select="comment"/>|
</xsl:for-each>
      </xsl:if>

      <xsl:if test="count(class) &gt; 0">
### Mapping

<xsl:for-each select="class">
#### <xsl:value-of select="@name"/><xsl:text>

</xsl:text>
<xsl:value-of select="comment"/>

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|<xsl:apply-templates select="id"/>
        <xsl:apply-templates select="version"/>
        <xsl:apply-templates select="property"/>
        <xsl:apply-templates select="many-to-one"/><xsl:text>

</xsl:text>
</xsl:for-each>
      </xsl:if>

    </xsl:template>

    <xsl:template match="managed-bean">|<xsl:value-of select="managed-bean-name"/>|<xsl:value-of select="description"/>|
</xsl:template>

    <xsl:template match="context-param">|<xsl:value-of select="param-name"/>|<xsl:call-template name="string-trim"><xsl:with-param name="string" select="normalize-space(description)"/></xsl:call-template>|
| |<xsl:value-of select="param-value"/>|
</xsl:template>

    <xsl:template match="entity-permission">|<xsl:value-of select="name"/>|<xsl:value-of select="entity-type"/>|<xsl:value-of select="description"/>|
</xsl:template>

    <xsl:template match="class">
#### <xsl:value-of select="@name"/><xsl:text>

</xsl:text>
<xsl:value-of select="comment"/>

| Type     | Ada      | Name     | Description   |
| ---------| ---------| ---------| ------------- |<xsl:apply-templates select="id"/>
        <xsl:apply-templates select="version"/>
        <xsl:apply-templates select="property"/>
        <xsl:apply-templates select="many-to-one"/><xsl:text>

</xsl:text>
    </xsl:template>

    <xsl:template match="property">
|<xsl:value-of select="column/@sql-type"/>|<xsl:value-of select="@type"/>|<xsl:value-of select="@name"/><xsl:value-of select="column/@name"/>|<xsl:value-of select="comment"/>|</xsl:template>

    <xsl:template match="id">
|<xsl:value-of select="column/@sql-type"/>|<xsl:value-of select="@type"/>|<xsl:value-of select="column/@name"/>|<xsl:value-of select="comment"/>|
</xsl:template>

    <xsl:template match="version">
|<xsl:value-of select="@type"/>| | <xsl:value-of select="@column"/>|<xsl:value-of select="comment"/>|
    </xsl:template>

    <xsl:template match="many-to-one">
|<xsl:value-of select="column/@sql-type"/>|<xsl:value-of select="@class"/>|<xsl:value-of select="column/@name"/>|<xsl:value-of select="comment"/>|
    </xsl:template>

    <xsl:template match="text()|@*"/>






<xsl:variable name="whitespace" select="'&#09;&#10;&#13; '" />

<!-- Strips trailing whitespace characters from 'string' -->
<xsl:template name="string-rtrim">
    <xsl:param name="string" />
    <xsl:param name="trim" select="$whitespace" />

    <xsl:variable name="length" select="string-length($string)" />

    <xsl:if test="$length &gt; 0">
        <xsl:choose>
            <xsl:when test="contains($trim, substring($string, $length, 1))">
                <xsl:call-template name="string-rtrim">
                    <xsl:with-param name="string" select="substring($string, 1, $length - 1)" />
                    <xsl:with-param name="trim"   select="$trim" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$string" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:if>
</xsl:template>

<!-- Strips leading whitespace characters from 'string' -->
<xsl:template name="string-ltrim">
    <xsl:param name="string" />
    <xsl:param name="trim" select="$whitespace" />

    <xsl:if test="string-length($string) &gt; 0">
        <xsl:choose>
            <xsl:when test="contains($trim, substring($string, 1, 1))">
                <xsl:call-template name="string-ltrim">
                    <xsl:with-param name="string" select="substring($string, 2)" />
                    <xsl:with-param name="trim"   select="$trim" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$string" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:if>
</xsl:template>

<!-- Strips leading and trailing whitespace characters from 'string' -->
<xsl:template name="string-trim">
    <xsl:param name="string" />
    <xsl:param name="trim" select="$whitespace" />
    <xsl:call-template name="string-rtrim">
        <xsl:with-param name="string">
            <xsl:call-template name="string-ltrim">
                <xsl:with-param name="string" select="$string" />
                <xsl:with-param name="trim"   select="$trim" />
            </xsl:call-template>
        </xsl:with-param>
        <xsl:with-param name="trim"   select="$trim" />
    </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
