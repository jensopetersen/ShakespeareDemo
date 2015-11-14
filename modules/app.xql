xquery version "3.0";

module namespace app="http://exist-db.org/apps/";

import module namespace templates="http://exist-db.org/xquery/templates";
import module namespace config="http://exist-db.org/apps/shakes/config" at "config.xqm";
import module namespace tei2="http://exist-db.org/xquery/app/tei2html" at "tei2html.xql";
import module namespace kwic="http://exist-db.org/xquery/kwic" at "resource:org/exist/xquery/lib/kwic.xql";
    
declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace functx = "http://www.functx.com";

(:modified by applying functx:escape-for-regex() :)
declare function functx:number-of-matches 
  ( $arg as xs:string? ,
    $pattern as xs:string )  as xs:integer {

   count(tokenize(functx:escape-for-regex($arg),functx:escape-for-regex($pattern))) - 1
 } ;

declare function functx:escape-for-regex
  ( $arg as xs:string? )  as xs:string {

   replace($arg,
           '(\.|\[|\]|\\|\||\-|\^|\$|\?|\*|\+|\{|\}|\(|\))','\\$1')
 } ;

(:~
 : List Shakespeare works
 :)
(:template function in index.html:)
declare 
    %templates:wrap
function app:list-works($node as node(), $model as map(*)) {
    map {
        "works" :=
            for $work in collection($config:data)/tei:TEI
            order by app:work-title($work)
            return
                $work
    }
};

(:template function in view-div.html, view-outline.html:)
declare
    %templates:wrap
function app:work($node as node(), $model as map(*), $id as xs:string?) {
    let $work := collection($config:data)//id($id)
    return
        map { "work" := $work }
};

(:template function in view-outline.html:)
declare function app:header($node as node(), $model as map(*)) {
    tei2:tei2html($model("work")/tei:teiHeader)
};

(:template function in view-div.html, view-outline.html. :)
declare function app:outline($node as node(), $model as map(*), $details as xs:string) {
    let $details := $details = "yes"
    let $work := $model("work")/ancestor-or-self::tei:TEI
    let $current := $model("work")
    return
        (:If the work is a play:)
        if ($work//tei:speaker)
        then
            <ul xmlns="http://www.w3.org/1999/xhtml">
            {
                for $act in $work/tei:text/tei:body/tei:div
                return
                    <li>{$act/tei:head/text()}
                        <ul>{
                            for $scene in $act/tei:div
                            let $class := if ($scene is $current) then "active" else ""
                            return
                                <li>
                                    {
                                        if ($details) then (
                                            <p><a href="{$scene/@xml:id}.html" class="{$class}">{$scene/tei:head/text()}</a></p>,
                                            <p>{$scene/tei:stage[1]/text()}</p>,
                                            
                                            if ($scene//tei:speaker)
                                            then
                                                <p><em>Speakers: </em>
                                                {
                                                    string-join(
                                                        for $speaker in distinct-values($scene//tei:speaker)
                                                        order by $speaker
                                                        return
                                                            $speaker
                                                    , ", ")
                                                }
                                                </p>
                                            else ()
                                        ) else
                                            <a href="{$scene/@xml:id}.html" class="{$class}">{$scene/tei:head/text()}</a>
                                    }
                                </li>
                        }</ul>
                    </li>
            }</ul>
        else
            (:If the work is Lover's Complaint, Phoenix and Turtle:)
            if ($work/tei:text/tei:body/tei:div/tei:lg/tei:l)
            then
                <table class="poem-list" xmlns="http://www.w3.org/1999/xhtml">
                {
                    for $stanza at $stanza-count in $work/tei:text/tei:body/tei:div/tei:lg
                    let $class := if ($stanza is $current) then "active" else ""
                    return
                        <tr>
                            <td><a href="{$stanza/@xml:id}.html" class="{$class}">Stanza {$stanza-count}</a></td> 
                            <td class="first-line">
                            {
                                if ($stanza/tei:lg/tei:l)
                                then $stanza/tei:lg[1]/tei:l[1]/text()
                                else
                                    if ($stanza/tei:l)
                                    then $stanza/tei:l[1]/text()
                                    else ''
                            }
                            </td>
                        </tr>
                }
                </table>
            else
                (:If the work is Rape of Lucrece, Venus and Adonis:) 
                if ($work/tei:text/tei:body/tei:div/tei:lg/tei:lg/tei:l)
                then
                    <table class="poem-list" xmlns="http://www.w3.org/1999/xhtml">
                    {
                        for $stanza in $work/tei:text/tei:body/tei:div/tei:lg
                        let $class := if ($stanza is $current) then "active" else ""
                        return
                            <tr>
                                <td><a href="{$stanza/@xml:id}.html" class="{$class}">Stanza {$stanza/@n/string()}</a></td> 
                                <td class="first-line">
                                {
                                    if ($stanza/tei:lg/tei:l)
                                    then $stanza/tei:lg[1]/tei:l[1]/text()
                                    else
                                        if ($stanza/tei:l)
                                        then $stanza/tei:l[1]/text()
                                        else 'WHAT?'
                                }
                                </td>
                            </tr>
                    }
                    </table>
                else
                    (:If the work is Sonnets.:)
                    <table class="poem-list" xmlns="http://www.w3.org/1999/xhtml">
                    {
                        for $sonnet in $work/tei:text/tei:body/tei:div/tei:div
                        let $class := if ($sonnet is $current) then "active" else ""
                        return
                            <tr>
                                <td><a href="{$sonnet/@xml:id}.html" class="{$class}">{$sonnet/tei:head/string()}</a></td> 
                                <td class="first-line">
                                {
                                    if ($sonnet/tei:lg/tei:l)
                                    then $sonnet/tei:lg[1]/tei:l[1]/text()
                                    else
                                        if ($sonnet/tei:l)
                                        then $sonnet/tei:l[1]/text()
                                        else 'WHAT?'
                                }
                                </td>
                            </tr>
                    }
                    </table>
};

(:~
 : 
 :)
(:template function in index.html:)
declare function app:work-title($node as node(), $model as map(*), $type as xs:string?) {
    let $suffix := if ($type) then "." || $type else ()
    let $work := $model("work")/ancestor-or-self::tei:TEI
    return
        <a xmlns="http://www.w3.org/1999/xhtml" href="{$node/@href}{$work/@xml:id}{$suffix}">{ app:work-title($work) }</a>
};

declare %private function app:work-title($work as element(tei:TEI)) {
    $work/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[1]/text()
};

(:template function in index.html:)
declare 
    %templates:wrap
function app:checkbox($node as node(), $model as map(*), $target-texts as xs:string*) {
    attribute { "value" } {
        $model("work")/@xml:id/string()
    },
    if ($model("work")/@xml:id/string() = $target-texts) then
        attribute checked { "checked" }
    else
        ()
};

(: template function in index.html:)
declare function app:work-type($node as node(), $model as map(*)) {
    let $work := $model("work")/ancestor-or-self::tei:TEI
    let $id := $work/@xml:id/string()
    let $work-types := doc(concat($config:data-root, '/', 'work-types.xml'))//item[id = $id]/value
    return 
        string-join(
            for $work-type in $work-types
            order by $work-type 
            return $work-type
        , ', ')    
};

(:template function in index.html:)
declare function app:epub-link($node as node(), $model as map(*)) {
    let $id := $model("work")/@xml:id/string()
    return
        <a xmlns="http://www.w3.org/1999/xhtml" href="{$node/@href}{$id}.epub">{ $node/node() }</a>
};

(:template function in index.html:)
declare function app:pdf-link($node as node(), $model as map(*)) {
    let $id := $model("work")/@xml:id/string()
    return
        <a xmlns="http://www.w3.org/1999/xhtml" href="{$node/@href}{$id}.pdf">{ $node/node() }</a>
};

(:template function in index.html:)
declare function app:xml-link($node as node(), $model as map(*)) {
    let $doc-path := document-uri(root($model("work")))
    let $eXide-link := templates:link-to-app("http://exist-db.org/apps/eXide", "index.html?open=" || $doc-path)
    let $rest-link := '/exist/rest' || $doc-path
    return
        if (xmldb:collection-available('/db/apps/eXide'))
        then <a xmlns="http://www.w3.org/1999/xhtml" href="{$eXide-link}" target="_blank">{ $node/node() }</a>
        else <a xmlns="http://www.w3.org/1999/xhtml" href="{$rest-link}" target="_blank">{ $node/node() }</a>
};

(:template function in search.html, view-div.html, view-outline.html:)
declare function app:copy-params($node as node(), $model as map(*)) {
    element { node-name($node) } {
        $node/@* except $node/@href,
        attribute href {
            let $link := $node/@href
            let $params :=
                string-join(
                    for $param in request:get-parameter-names()
                    for $value in request:get-parameter($param, ())
                    return
                        $param || "=" || $value,
                    "&amp;"
                )
            return
                $link || "?" || $params
        },
        $node/node()
    }
};

(:template function in index.html:)
declare function app:work-types($node as node(), $model as map(*)) {
    let $types := distinct-values(doc(concat($config:data-root, '/', 'work-types.xml'))//value)
    let $control :=
        <select multiple="multiple" name="work-types" class="form-control">
            <option value="all">All Work Types</option>
            {for $type in $types
            return <option value="{$type}">{$type}</option>
            }
        </select>
    return
        templates:form-control($control, $model)
};

(:template function in view-div.html:)
declare function app:navigation($node as node(), $model as map(*)) {
    let $div := $model("work")
    let $prevDiv := $div/preceding::tei:div[parent::tei:div][1]
    let $nextDiv := $div/following::tei:div[parent::tei:div][1]
    let $work := $div/ancestor-or-self::tei:TEI
    return
        element { node-name($node) } {
            $node/@*,
            if ($prevDiv) then
                <a xmlns="http://www.w3.org/1999/xhtml" href="{$prevDiv/@xml:id}.html" class="previous">
                    <i class="glyphicon glyphicon-chevron-left"/> Previous Scene</a>
            else
                (),
            if ($nextDiv) then
                <a xmlns="http://www.w3.org/1999/xhtml" href="{$nextDiv/@xml:id}.html" class="next">
                    Next Scene <i class="glyphicon glyphicon-chevron-right"/></a>
            else
                (),
            <h5 xmlns="http://www.w3.org/1999/xhtml"><a href="{$work/@xml:id}">{app:work-title($work)}</a></h5>
        }
};

(:template function in view-div.html:)
declare 
    %templates:default("action", "browse")
function app:view-div($node as node(), $model as map(*), $id as xs:string, $action as xs:string) {
    let $query := 
        if ($action eq 'search')
        then session:get-attribute("apps.shakespeare.query")
        else ()
    let $scope := 
        if ($query) 
        then session:get-attribute("apps.shakespeare.scope") 
        else ()
    let $div :=$model("work")/id($id)
    let $div :=
        if ($query) then
            if ($scope eq 'narrow') then
                util:expand(($div[.//tei:sp[ft:query(., $query)]], $div[.//tei:lg[ft:query(., $query)]]), "add-exist-id=all")
            else
                util:expand($div[ft:query(., $query)], "add-exist-id=all")
        else
            $div
    return
        <div xmlns="http://www.w3.org/1999/xhtml" class="play">
        { tei2:tei2html($div) }
        </div>
};

(:~
    Execute the query. The search results are not output immediately. Instead they
    are passed to nested templates through the $model parameter.
:)
(:template function in search.html:)
declare 
    %templates:default("scope", "narrow")
    %templates:default("work-types", "all")
    %templates:default("target-texts", "all")
function app:query($node as node()*, $model as map(*), $query as xs:string?, $scope as xs:string, 
    $work-types as xs:string+, $target-texts as xs:string+) as map() {
    let $query := if ($query) then app:sanitize-lucene-query($query) else ''
    let $query := normalize-space($query)
    return
        if ($query = "") then
            let $cached := session:get-attribute("apps.shakespeare")
            return
                map {
                    "hits" := $cached,
                    "query" := session:get-attribute("apps.shakespeare.query"),
                    "scope" := $scope,
                    "target-texts" := $target-texts
                }
        else
            (:Get the work ids of the work types selected.:)  
            let $target-text-ids := distinct-values(doc(concat($config:data-root, '/', 'work-types.xml'))//item[value = $work-types]/id)
            (:If no individual works have been selected, search in the works with ids selected by type;
            if indiidual works have been selected, then neglect that no selection has been done in works according to type.:) 
            let $target-texts := 
                if ($target-texts = 'all' and $work-types = 'all')
                then 'all' 
                else 
                    if ($target-texts = 'all')
                    then $target-text-ids
                    else 
                        if ($work-types = "all") then $target-texts else ($target-texts[. = $target-text-ids])
            let $context := 
                if ($target-texts = 'all')
                then collection($config:data-root)/tei:TEI
                else collection($config:data-root)//tei:TEI[@xml:id = $target-texts]
            let $hits :=
                if ($scope eq 'narrow')
                then
                    for $hit in ($context//tei:sp[ft:query(., $query)], $context//tei:lg[ft:query(., $query)])
                    order by ft:score($hit) descending
                    return $hit
                else
                    for $hit in $context//tei:div[not(tei:div)][ft:query(., $query)]
                    order by ft:score($hit) descending
                    return $hit
            let $store := (
                session:set-attribute("apps.shakespeare", $hits),
                session:set-attribute("apps.shakespeare.query", $query),
                session:set-attribute("apps.shakespeare.scope", $scope),
                session:set-attribute("apps.shakespeare.target-texts", $target-texts)
            )
            return
                (: Process nested templates :)
                map {
                    "hits" := $hits,
                    "query" := $query,
                    "scope" := $scope,
                    "target-texts" := $target-texts
                }
};

(:~
 : Create a bootstrap pagination element to navigate through the hits.
 :)
(:template function in search.html:)
declare
    %templates:wrap
    %templates:default('start', 1)
    %templates:default("per-page", 10)
    %templates:default("min-hits", 0)
    %templates:default("max-pages", 10)
function app:paginate($node as node(), $model as map(*), $start as xs:int, $per-page as xs:int, $min-hits as xs:int,
    $max-pages as xs:int) {
    if ($min-hits < 0 or count($model("hits")) >= $min-hits) then
        let $count := xs:integer(ceiling(count($model("hits"))) div $per-page) + 1
        let $middle := ($max-pages + 1) idiv 2
        return (
            if ($start = 1) then (
                <li class="disabled">
                    <a><i class="glyphicon glyphicon-fast-backward"/></a>
                </li>,
                <li class="disabled">
                    <a><i class="glyphicon glyphicon-backward"/></a>
                </li>
            ) else (
                <li>
                    <a href="?start=1"><i class="glyphicon glyphicon-fast-backward"/></a>
                </li>,
                <li>
                    <a href="?start={max( ($start - $per-page, 1 ) ) }"><i class="glyphicon glyphicon-backward"/></a>
                </li>
            ),
            let $startPage := xs:integer(ceiling($start div $per-page))
            let $lowerBound := max(($startPage - ($max-pages idiv 2), 1))
            let $upperBound := min(($lowerBound + $max-pages - 1, $count))
            let $lowerBound := max(($upperBound - $max-pages + 1, 1))
            for $i in $lowerBound to $upperBound
            return
                if ($i = ceiling($start div $per-page)) then
                    <li class="active"><a href="?start={max( (($i - 1) * $per-page + 1, 1) )}">{$i}</a></li>
                else
                    <li><a href="?start={max( (($i - 1) * $per-page + 1, 1)) }">{$i}</a></li>,
            if ($start + $per-page < count($model("hits"))) then (
                <li>
                    <a href="?start={$start + $per-page}"><i class="glyphicon glyphicon-forward"/></a>
                </li>,
                <li>
                    <a href="?start={max( (($count - 1) * $per-page + 1, 1))}"><i class="glyphicon glyphicon-fast-forward"/></a>
                </li>
            ) else (
                <li class="disabled">
                    <a><i class="glyphicon glyphicon-forward"/></a>
                </li>,
                <li>
                    <a><i class="glyphicon glyphicon-fast-forward"/></a>
                </li>
            )
        ) else
            ()
};

(:~
    Create a span with the number of items in the current search result.
:)
(:template function in search.html:)
declare function app:query-report($node as node()*, $model as map(*)) {
    let $hit-count := count($model("hits"))
    let $ids := $model("target-texts")
    return
    <span xmlns="http://www.w3.org/1999/xhtml" id="query-report"> You searched for <strong>{'"' || $model("query") || '" '}</strong> in <strong>{if ($ids eq 'all') then 'all works' else app:ids-to-titles($ids)}</strong> and found <strong>{$hit-count}</strong>{if ($hit-count eq 1) then ' match.' else ' matches.'}
    </span>
};

declare function app:ids-to-titles($ids as xs:string+) {
    let $titles :=
        for $id in $ids
        return
            collection($config:data-root)//tei:TEI[@xml:id = $id]//tei:titleStmt/tei:title
    let $count := count($titles)
    return
        app:serialize-list($titles, $count)
};

declare function app:serialize-list($sequence as item()+, $sequence-count as xs:integer) as xs:string {       
    if ($sequence-count eq 1)
        then $sequence
        else
            if ($sequence-count eq 2)
            then concat(
                subsequence($sequence, 1, $sequence-count - 1),
                (:Places " and " before last item.:)
                ' and ',
                $sequence[$sequence-count]
                )
            else concat(
                (:Places ", " after all items that do not come last.:)
                string-join(subsequence($sequence, 1, $sequence-count - 1)
                , ', ')
                ,
                (:Places ", and " before item that comes last.:)
                ', and ',
                $sequence[$sequence-count]
                )
};

(:~
    Output the actual search result as a div, using the kwic module to summarize full text matches.
:)
(:template function in ajax.html, search.html:)
declare 
    %templates:wrap
    %templates:default("start", 1)
    %templates:default("per-page", 10)
function app:show-hits($node as node()*, $model as map(*), $start as xs:integer, $per-page as xs:integer) {
    for $hit at $p in subsequence($model("hits"), $start, $per-page)
    let $div := $hit/ancestor-or-self::tei:div[1]
    let $div-id := $div/@xml:id/string()
    let $div-head := $div/tei:head/text() 
    let $work := $hit/ancestor::tei:TEI
    let $work-id := $work/@xml:id/string()
    let $work-title := app:work-title($work)
    (:pad hit with surrounding siblings in order to get more context:)
    let $matchId := ($hit/@xml:id, util:node-id($hit))[1]
    let $loc := 
        <tr class="reference">
            <td colspan="3">
                <span class="number">{$start + $p - 1}</span>
                <a href="{$work-id}.html">{$work-title}</a>, <a href="{$div-id}.html?action=search">{$div-head}</a>
            </td>
        </tr>
    let $config := <config width="100" table="yes" link="{$div-id}.html?action=search#{$matchId}"/>
    let $hit := <hit>{($hit/preceding-sibling::*[1], $hit, $hit/following-sibling::*[1])}</hit>
    let $hit := util:expand($hit)
    return
        for $match in $hit//exist:match
        let $kwic := kwic:get-summary($hit, $match, $config, app:filter#2)
        return ($loc, $kwic)
};

(:~
    Callback function called from the kwic module.
:)
declare %private function app:filter($node as node(), $mode as xs:string) as xs:string? {
  if ($node/parent::tei:speaker or $node/parent::tei:stage or $node/parent::tei:head) then 
      concat('(', $node, ':) ')
  else if ($mode eq 'before') then 
      concat($node, ' ')
  else 
      concat(' ', $node)
};

(: This functions provides crude way to avoid the most common errors with paired expressions and apostrophes. :)
(: TODO: check order of pairs:)
declare %private function app:sanitize-lucene-query($query-string as xs:string) as xs:string {
    let $query-string := replace($query-string, "'", "''") (:escape apostrophes:)
    (:TODO: notify user if query has been modified.:)
    let $query-string := translate($query-string, ":", "") (:remove colons – Lucene fields are not supported.:)
    let $query-string := 
	   if (functx:number-of-matches($query-string, '"') mod 2)
	   then '"' || replace($query-string, '"', '') || '"'(:if there is an uneven number of quotation marks, delete all quotation marks and apply them again.:)
	   else $query-string
    let $query-string := 
	   if (functx:number-of-matches($query-string, '/') mod 2)
	   then "/" || translate($query-string, '/', '') || "/" (:if there is an uneven number of slashes, delete all slashes and apply them again.:)
	   else $query-string
    let $query-string := 
	   if ((functx:number-of-matches($query-string, '\(') + functx:number-of-matches($query-string, '\)')) mod 2)
	   then translate($query-string, '()', '') (:if there is an uneven number of parentheses, delete all parentheses.:)
	   else $query-string
    let $query-string := 
	   if ((functx:number-of-matches($query-string, '\[') + functx:number-of-matches($query-string, '\]')) mod 2)
	   then translate($query-string, '[]', '') (:if there is an uneven number of brackets, delete all brackets.:)
	   else $query-string
    let $query-string := 
	   if ((functx:number-of-matches($query-string, '{') + functx:number-of-matches($query-string, '}')) mod 2)
	   then translate($query-string, '{}', '') (:if there is an uneven number of braces, delete all braces.:)
	   else $query-string
    let $query-string := 
	   if ((functx:number-of-matches($query-string, '<') + functx:number-of-matches($query-string, '>')) mod 2)
	   then translate($query-string, '<>', '') (:if there is an uneven number of angle brackets, delete all angle brackets.:)
	   else $query-string
    return $query-string
};