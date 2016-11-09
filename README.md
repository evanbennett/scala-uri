# scala-uri

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/NET-A-PORTER/scala-uri?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.netaporter/scala-uri_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.netaporter/scala-uri_2.12)
[![Build Status](https://travis-ci.org/NET-A-PORTER/scala-uri.svg?branch=master)](https://travis-ci.org/NET-A-PORTER/scala-uri)
[![codecov.io](http://codecov.io/github/NET-A-PORTER/scala-uri/coverage.svg?branch=master)](http://codecov.io/github/NET-A-PORTER/scala-uri?branch=master)

**scala-uri** is a Scala library that helps you work with URIs. It has the following features:

 * [RFC 3986](https://tools.ietf.org/html/rfc3986) compliance when using `UriConfig.RFC3986`
 * [URI syntax normalization](#uri-syntax-normalization) consistent with [RFC 3986](https://tools.ietf.org/html/rfc3986), with options including:
   * [Empty component normalization](#empty-component-normalization) (where empty authority components will be removed)
   * [Case normalization](#case-normalization) (where the scheme and host will be converted to lower case when necessary)
   * [Percent encoding normalization](#percent-encoding-normalization) (can be enabled and each available component can be individually set)
   * [Path segment normalization](#path-segment-normalization) (where dot segments will be removed in relative paths)
 * [URI relative resolution](#uri-relative-resolution) consistent with [RFC 3986](https://tools.ietf.org/html/rfc3986)
 * A parser for [parsing URIs](#parsing-uris) from Strings, with options including:
   * [RFC 3986 compliance](#rfc-3986-compliant-parsing)
   * [Delimiter parsing](#delimiter-parsing) (where the component contents are not checked)
   * [The registered name must be a domain name](#registered-name-must-be-a-domain-name-during-parsing)
   * [Matrix parameter parsing](#matrix-parameter-parsing) (where the path segments will have any matrix parameters extracted)
   * [Query parameter parsing](#query-parameter-parsing) (where the query string will be parsed as parameters)
   * [Fragment allowing '#' parsing](#fragment-allowing-parsing) (where the fragment may contain '#'s)
   * Some components may be [decoded](#percent-decoding) (and each available component can be individually set)
 * A DSL for [building URIs](#building-uris-with-the-dsl)
 * No dependencies on existing web frameworks
 * // THEON: I removed "protocol relative urls" as they are just `NetworkPathReference`s and according to the link "now an anti-pattern". They still work as they are part of RFC 3986 (and can now be resolved), but I think they do not need to be explicitly referenced any more.
 * Support for [user information](#user-information) (e.g. "ftp://user:password@mysite.com")
 * Support for extracting TLDs and [public suffixes](#registered-name-public-suffixes) such as ".com" and ".co.uk" from a registered name
 * Support for [matrix parameters](#matrix-parameters)
 * Support for [custom encoding](#custom-encoding) such as encoding [spaces as pluses](#encoding-spaces-as-pluses)

There is also a [demo project](https://github.com/NET-A-PORTER/scala-uri-demo) to help you get up and running quickly from scratch.

*Note:* This library works best when using Scala 2.11.2+. Due a bug in older versions of Scala, this library can result in `StackOverflowException`s for very large URIs when using versions of Scala older than 2.11.2. [More details](https://github.com/NET-A-PORTER/scala-uri/issues/51#issuecomment-45759462)

## Including in your project

**scala-uri** 1.0.0-RC1 is currently published with support for Scala 2.10, 2.11 and 2.12.

Release builds are available from [Maven Central](https://search.maven.org/#search%7Cga%7C1%7Cscala-uri).

For SBT users just add the following dependency:

```scala
"com.netaporter" %% "scala-uri" % "1.0.0-RC1"
```

For Maven users you should use (for 2.12):

```xml
<dependency>
    <groupId>com.netaporter</groupId>
    <artifactId>scala-uri_2.12</artifactId>
    <version>1.0.0-RC1</version>
</dependency>
```

### Latest snapshot builds

// THEON: How is this section used?

For the latest snapshot builds, add the Sonatype OSS snapshots repository to your SBT build configuration:

```scala
resolvers += "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/snapshots"
```

And add the following dependency:

```scala
"com.netaporter" %% "scala-uri" % "1.0.0-SNAPSHOT"
```

## Parsing URIs

The simplest way to parse a URI is to use the `Uri` constructor which takes a `String`:

```scala
val uri = Uri("http://theon.github.com/scala-uri?param1=1&param2=2")
```

Or, if you prefer an `Option[Uri]`, you can call `Uri.option`:

```scala
val uri = Uri.option("http://theon.github.com/scala-uri?param1=1&param2=2")
```

### RFC 3986 compliant parsing

For [RFC 3986](https://tools.ietf.org/html/rfc3986) compliant parsing, simply provide an implicit `UriConfig.RFC3986`:

```scala
implicit val config = UriConfig.RFC3986
val uri = Uri("http://theon.github.com/scala-uri?param1=1&param2=2")
```

### Delimiter parsing

Delimiter parsing removes all restriction on each component's contents, except that it cannot contain any characters that would be considered a delimiter of the component.

For delimiter parsing, simply provide an implicit `UriConfig` with `delimiterParsing = true`:

```scala
implicit val config = UriConfig(delimiterParsing = true)
val uri = Uri("http://theon.github.com/scala-uri?param1=1&param2=2")
```

### User and password parsing

User and password parsing can be enabled by providing an implicit `UriConfig` with `userPasswordParsing = true`:

```scala
implicit val config = UriConfig(userPasswordParsing = true)
val uri = Uri("http://user:password@theon.github.com/scala-uri")
```

### Registered name must be a domain name during parsing

**Note:** This is enable by default in `UriConfig` and `UriConfig.DEFAULT`.

The most common name registry mechanism is the Domain Name System (DNS). When you want to, you can restrict a registered name to a domain name. To enable this, provide an implicit `UriConfig` with `registeredNameMustBeDomainName = true`:

```scala
implicit val config = UriConfig.RFC3986.copy(registeredNameMustBeDomainName = true)
val uri = Uri("http://theon.github.com/scala-uri?param1=1&param2=2")
```

### Matrix parameter parsing

[Matrix parameter](#matrix-parameters) parsing can be enabled by providing an implicit `UriConfig` with `matrixParameterParsing = true`:

```scala
implicit val config = UriConfig(matrixParameterParsing = true)
val uri = Uri("http://theon.github.com/scala-uri;param1=1;param2=2")
```

### Query parameter parsing

**Note:** This is enable by default in `UriConfig` and `UriConfig.DEFAULT`.

Query parameter parsing can be enabled by providing an implicit `UriConfig` with `queryParameterParsing = true`:

```scala
implicit val config = UriConfig(queryParameterParsing = true)
val uri = Uri("http://theon.github.com/scala-uri?param1=1&param2=2")
```

### Fragment allowing '#' parsing

**Note:** This is enable by default in `UriConfig` and `UriConfig.DEFAULT`.

While [RFC 3986](https://tools.ietf.org/html/rfc3986) does not allow the fragment to contain '#'s, URIs that have multiple '#'s appear to occur with some regularity. To enable this, provide an implicit `UriConfig` with `fragmentAllowHashParsing = true`:

```scala
implicit val config = UriConfig.RFC3986.copy(fragmentAllowHashParsing = true)
val uri = Uri("http://theon.github.com/scala-uri?param1=1&param2=2#fragment1#fragment2")
```

## Building URIs with the DSL

TODO: Once the direction of the DSL is decided, I will update this section.

### Path

```scala
import com.netaporter.uri.dsl._
val uri = "http://theon.github.com" / "scala-uri"
uri.toString // "http://theon.github.com/scala-uri"
```

To add path segments, use the `/` method

### Query

```scala
import com.netaporter.uri.dsl._
val uri = "http://theon.github.com/scala-uri" ? ("p1" -> "one") & ("p2" -> 2) & ("p3" -> true)
uri.toString // "http://theon.github.com/scala-uri?p1=one&p2=2&p3=true"

val uri2 = "http://theon.github.com/scala-uri" ? ("param1" -> Some("1")) & ("param2" -> None)
uri2.toString // "http://theon.github.com/scala-uri?param1=1"
```

To add query string parameters, use either the `?` or `&` method and pass a `Tuple2` as an argument. The first value in the Tuple is a name of the query string parameter, the second is the value. If a parameter value is an `Option`, it will only be rendered provided it is not `None`.

### Fragment

To set the fragment, use the `` `#` `` method:

```scala
import com.netaporter.uri.dsl._
val uri = "http://theon.github.com/scala-uri" `#` "fragment"
uri.toString // "http://theon.github.com/scala-uri#fragment"
```

## URI syntax normalization

### Empty component normalization

**Note:** This is enable by default in `UriConfig`, `UriConfig.DEFAULT` and `UriConfig.RFC3986` and `UriConfig.CONSERVATIVE`.

When enabled, empty user, password and port components will be removed when `toString` is called. To disable this, provide an implicit `UriConfig` with `emptyComponentNormalization = false`:

```scala
implicit val config = UriConfig(emptyComponentNormalization = false)
```

### Case normalization

**Note:** This is enable by default in `UriConfig`, `UriConfig.DEFAULT` and `UriConfig.RFC3986` and `UriConfig.CONSERVATIVE`.

When enabled, ensure that the scheme and host are lower case when `toString` is called. To disable this, provide an implicit `UriConfig` with `caseNormalization = false`:

```scala
implicit val config = UriConfig(caseNormalization = false)
```

### Percent encoding normalization

**Note:** This is enable by default in `UriConfig`, `UriConfig.DEFAULT` and `UriConfig.RFC3986` and `UriConfig.CONSERVATIVE`.

When enabled, decode when parsing and encode when `toString` is called, using the `*Decoder` and `*Encoder` options in the `UriConfig`. To disable this, provide an implicit `UriConfig` with `percentEncodingNormalization = false`:

```scala
implicit val config = UriConfig(percentEncodingNormalization = false)
```

### Path segment normalization

**Note:** This is enable by default in `UriConfig`, `UriConfig.DEFAULT` and `UriConfig.RFC3986` and `UriConfig.CONSERVATIVE`.

When enabled, remove dot segments ("." and "..") in non-relative paths when constructing `Uri`s. To disable this, provide an implicit `UriConfig` with `pathSegmentNormalization = false`:

```scala
implicit val config = UriConfig(pathSegmentNormalization = false)
```

## URI relative resolution

A `Uri` can be resolved, relative to a base `AbsoluteUri`, to produce the target URI. The algorithm used can be found in [RFC 3986 section 5.2](http://tools.ietf.org/html/rfc3986#section-5.2).

```scala
Uri("http://theon.github.com/existingPath/") match {
  case baseUri: AbsoluteUri =>
    val relativeUri = Uri("newPath?queryKey=value#fragment")
    val targetUri = baseUri.resolve(relativeUri)
    targetUri.toString // "http://theon.github.com/existingPath/newPath?queryKey=value#fragment"
}
```

## User information

User information support includes the user and password.

**Note:** Using clear text passwords in URIs is [deprecated in RFC 3986](http://tools.ietf.org/html/rfc3986#section-3.2.1).

Accessing user information:

```scala
val uri = Uri("http://user:password@host.com")
uri.user // Some("user")
uri.password // Some("password")
```

Modifying user information:

```scala
val uri = Uri("http://user:password@host.com")
uri.withUser("jack").toString // "http://jack:password@host.com"
uri.withPassword("secret").toString // "http://user:secret@host.com"
```

## Registered name must be a domain name

**Note:** This is enable by default in `UriConfig` and `UriConfig.DEFAULT`.

The most common name registry mechanism is the Domain Name System (DNS). When you want to, you can restrict a registered name to a domain name. This option will restrict the [registered name during parsing](#registered-name-must-be-a-domain-name-during-parsing) and when constructing a `Host`. To enable this, provide an implicit `UriConfig` with `registeredNameMustBeDomainName = true`:

```scala
implicit val config = UriConfig.RFC3986.copy(registeredNameMustBeDomainName = true)
Host.parse("www.google.com.au")
```

## Registered name public suffixes

**scala-uri** uses the list of public suffixes from [publicsuffix.org](https://publicsuffix.org) to allow you to identify the TLD of your URIs with a registered name.

The `registeredNamePublicSuffix` method returns the longest public suffix from your URI:

```scala
val uri = Uri("http://www.google.co.uk/blah")
uri.registeredNamePublicSuffix == Some("co.uk")
```

The `registeredNamePublicSuffixes` method returns all the public suffixes from your URI:

```scala
uri.registeredNamePublicSuffixes == Seq("co.uk", "uk")
```

These methods return `None` and `Seq.empty`, respectively for any URIs without a registered name.

## Matrix parameters

[Matrix Parameters](http://www.w3.org/DesignIssues/MatrixURIs.html) are supported in **scala-uri**. Support is enabled by using a `UriConfig` with `matrixParameterParsing = true` like so:

```scala
implicit val config = UriConfig(matrixParameterParsing = true)
val uri = Uri("http://example.com/path;paramOne=value;paramTwo=value2/pathTwo;paramThree=value3")

You can get the parameters of the last path segment:

```scala
uri.matrixParametersOfLastSegment // Seq(Parameter("paramThree", Some("value3"))
```

Append a parameter to the last path segment:

```scala
val uri2 = uri.appendMatrixParameterToLastSegment("paramFour", "value4")
uri2.toString // "http://example.com/path;paramOne=value;paramTwo=value2/pathTwo;paramThree=value3;paramFour=value4"
```

Get the parameters of a specified path segment:

```scala
uri.pathSegment("path").parameters // Seq(Parameter("paramOne", Some("value")), Parameter("paramTwo", Some("value2"))
```

Append a parameter to a specified path segment:

```scala
val uri3 = uri.appendMatrixParameter("path", "paramFour", "value4")
uri3.toString // "http://example.com/path;paramOne=value;paramTwo=value2;paramFour=value4/pathTwo;paramThree=value3"
```

// THEON: I removed the sections relating to Queries, as I feel that that functionality is not special.

## Percent decoding

By default **scala-uri** will percent decode the user, password, registered name, path segments, query parameters and fragment during parsing:

```scala
val uri = Uri("http://example.com/i-have-%25been%25-percent-encoded")
uri.pathSegments.head // StringSegment("i-have-%been%-percent-encoded")
uri.toString // "http://example.com/i-have-%25been%25-percent-encoded"
```

To prevent this, you can create a custom implicit `UriConfig`:

```scala
implicit val config = UriConfig.DEFAULT.withNoDecoding
val uriNoDecoding = Uri("http://example.com/i-have-%25been%25-percent-encoded")
uri.pathSegments.head // StringSegment("i-have-%25been%25-percent-encoded")
uriNoDecoding.toString // "http://example.com/i-havent-%2525been%2525-percent-encoded"
```

## Percent encoding

By default, **scala-uri** will percent encode the user, password, registered name, path segments, query parameters and fragment when `toString` is called:

```scala
val uri = Uri("http://example.com/path with space?param=üri")(UriConfig(delimiterParsing = true))
uri.toString // "http://example.com/path%20with%20space?param=%C3%BCri"
```

Percent encoding can be disabled:

```scala
val uri = Uri("http://example.com/path with space?param=üri")(UriConfig(delimiterParsing = true))
uri.toString(UriConfig(percentEncodingNormalization = false)) // "http://example.com/path with space?param=üri"
```

You can modify which characters are percent encoded. For example, to only percent encode the hash character:

```scala
import com.netaporter.uri.encoding.PercentEncoder
implicit val config = UriConfig.DEFAULT.withEncoding(PercentEncoder('#'))
```

### Encoding spaces as pluses

The default behaviour with **scala-uri**, is to encode spaces as "%20", however if you instead wish them to be encoded as the "+" symbol, then simply add the following `implicit val` to your code:

```scala
val uri = Uri("http://theon.github.com/uri with space")(UriConfig(delimiterParsing = true))
import com.netaporter.uri.encoding._
implicit val config = UriConfig.DEFAULT.withEncoding(PercentEncoder(PercentEncoder.CharsetsToEncode.REGISTERED_NAME) + EncodeCharAs.SPACE_AS_PLUS)
uri.toString // "http://theon.github.com/uri+with+space"
```

### Custom encoding

If you would like to do some custom encoding for specific characters, you can use the `EncodeCharAs` encoder.

```scala
val uri = Uri("http://theon.github.com/uri with space")(UriConfig(delimiterParsing = true))
import com.netaporter.uri.encoding._
implicit val config = UriConfig.DEFAULT.withEncoding(PercentEncoder(PercentEncoder.CharsetsToEncode.REGISTERED_NAME) + EncodeCharAs(' ', "_"))
uri.toString // "http://theon.github.com/uri_with_space"
```

## Character set

By default **scala-uri** uses "UTF-8" character encoding:

```scala
val uri = Uri("http://theon.github.com/uris-in-scala.html").queryAppend("chinese", "网址")
uri.toString // "http://theon.github.com/uris-in-scala.html?chinese=%E7%BD%91%E5%9D%80"
```

This can be changed like so:

```scala
implicit val config = UriConfig(charset = java.nio.charset.Charset.forName("GB2312"))
val uriGb = Uri("http://theon.github.com/uris-in-scala.html").queryAppend("chinese", "网址")
uriGb.toString // "http://theon.github.com/uris-in-scala.html?chinese=%CD%F8%D6%B7"
```

## Contributions

Contributions to **scala-uri** are always welcome. Good ways to contribute include:

 * Raising bugs and feature requests
 * Fixing bugs and developing new features (I will attempt to merge in pull requests ASAP)
 * Improving the performance of **scala-uri**: see the [Performance Tests](#performance-tests)

## Building

### Unit tests

The unit tests can be run from the sbt console by running the `test` command. Checking the unit tests all pass before sending pull requests will be much appreciated.

Generate code coverage reports from the sbt console by running the `coverage`, `clean`, `test` and then `coverageReport` commands. The HTML reports should be generated at "target/scala-2.12/scoverage-report/index.html". Ideally pull requests shouldn't significantly decrease code coverage, but it's not the end of the world if they do. Contributions with no tests are better than no contributions. :)

### Performance tests

For the **scala-uri** performance tests, head to the [scala-uri-benchmarks](https://github.com/net-a-porter/scala-uri-benchmarks) github project.

## Migration guide

### From 1.0.x to 1.1.x

 * Remove all deprecation warnings before migrating

### From 0.4.x to 1.0.x

 * `Uri` method changes:
   * `scheme` use `protocol` instead
   * `path` use `pathToString` instead
   * `query` use `queryValue` instead
   * `fragment` use `fragmentString` instead
   * `queryString` use `queryToString` instead
   * `copy` use `copyOld` instead
   * `unapply` returns the new arguments
 * Incompatibilities:
   * An empty query (with no parameters) now `toString`s as "?", and `Uri` can have no query
   * A `Parameter` with no value now `toString`s with the key (e.g. "queryKey")
   * A `Parameter` with an empty value ("") now `toString`s with the key and '=' (e.g. "queryKey=")
 * `UriConfig` changes:
   * Moved to the main `uri` package to limit required `import`s
   * `matrixParams` has been renamed to `matrixParameterParsing`
   * `charset` has changed from a `String` to a `java.nio.charset.Charset`
   * `emptyComponentNormalization`, `caseNormalization`, `percentEncodingNormalization`, `pathSegmentNormalization`, `delimiterParsing`, `userPasswordParsing`, `registeredNameMustBeDomainName`, `queryParameterParsing`, `fragmentAllowHashParsing`, `userDecoder`, `passwordDecoder`, `registeredNameDecoder`, `userEncoder`, `passwordEncoder` and `registeredNameEncoder` members have been added
   * The default `apply` method now has the best available defaults
 * Parsing and DSL have been deprecated, and new version implemented
 * You may experience lots of deprecation warnings

### From 0.3.x to 0.4.x

 * Package changes / import changes:
   * All code moved from `com.github.theon` package to `com.netaporter` package
   * **scala-uri** has been organised into the following packages: `encoding`, `decoding`, `config` and `dsl` (You will need to update import statements.)
 * Name changes:
   * `PermissiveDecoder` renamed to `PermissivePercentDecoder`
   * `QueryString` and `MatrixParams` constructor argument `parameters` shortened to `params`
   * `Uri.parseUri` renamed to `Uri.parse`
   * `protocol` constructor arg in `Uri` renamed to `scheme`
   * `Querystring` renamed to `QueryString`
 * Query String constructor argument `parameters` changed type from `Map[String, List[String]]` to `Seq[(String,String)]`
 * `Uri` constructor argument `pathParts` changed type from `List` to `Vector`
 * `Uri` method to add query string parameters renamed from `params` to `addParams` (same with `matrixParams` -> `addMatrixParams`)
 * `PercentEncoderDefaults` object renamed to `PercentEncoder` companion object
 * Copy methods `user`/`password`/`port`/`host`/`scheme` now all prefixed with `with` (e.g. `withHost`)
 * New `UriConfig` case class used to specify encoders, decoders and charset to be used (see examples in [Percent decoding](#percent-decoding), [Percent encoding](#percent-encoding) and [Character set](#character-set))

## License

**scala-uri** is open source software released under the [Apache 2 License](http://www.apache.org/licenses/LICENSE-2.0).
