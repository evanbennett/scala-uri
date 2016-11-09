package com.netaporter.uri

class PathTests extends TestSpec {

  "`Uri.pathSegments`" should "return all the path segments" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), StringSegment("path2"), EmptySegment), None, None)
    uri.pathSegments should equal(Seq(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), StringSegment("path2"), EmptySegment))
  }

  it should "return empty when the path is empty" in {
    EmptyReference.pathSegments should equal(Seq.empty)
  }

  "`Uri.pathSegmentOption`" should "return the specified path segment" in {
    val path1 = MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1")))
    val uri = Uri(None, None, AbsolutePath.option(path1, StringSegment("path2"), EmptySegment), None, None)
    uri.pathSegmentOption("path1").value should equal(path1)
  }

  it should "return `None` when the specified segment does not exists" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), StringSegment("path2"), EmptySegment), None, None)
    uri.pathSegmentOption("path") should equal(None)
  }

  it should "return `None` when the path is empty" in {
    EmptyReference.pathSegmentOption("path") should equal(None)
  }

  "`Uri.pathSegment`" should "return the specified path segment" in {
    val path1 = MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1")))
    val uri = Uri(None, None, AbsolutePath.option(path1, StringSegment("path2"), EmptySegment), None, None)
    uri.pathSegment("path1") should equal(path1)
  }

  it should "fail when the specified segment does not exists" in {
    an [NoSuchElementException] should be thrownBy {
      val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), StringSegment("path2"), EmptySegment), None, None)
      uri.pathSegment("path") should equal(Seq.empty)
    }
  }

  it should "fail when the path is empty" in {
    an [NoSuchElementException] should be thrownBy {
      EmptyReference.pathSegment("path") should equal(Seq.empty)
    }
  }

  "`Uri.matrixParameters(String)`" should "return the matrix parameters of the specified segment only" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), MatrixParametersSegment("path2", Parameter("matrixParamKey2", Some("matrixParamValue2")), Parameter("matrixParamKey3", None)), EmptySegment), None, None)
    uri.matrixParameters("path2") should equal(Seq(Parameter("matrixParamKey2", Some("matrixParamValue2")), Parameter("matrixParamKey3", None)))
  }

  it should "return `None` when the specified segment is `StringSegment`" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), StringSegment("path2"), EmptySegment), None, None)
    uri.matrixParameters("path2") should equal(Seq.empty)
  }

  it should "return `None` when the specified segment is empty" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), EmptySegment), None, None)
    uri.matrixParameters("") should equal(Seq.empty)
  }

  it should "return `None` when the path is empty" in {
    EmptyReference.matrixParameters("path2") should equal(Seq.empty)
  }

  "`Uri.matrixParametersOfLastSegment`" should "return the matrix parameters of the last segment only" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), MatrixParametersSegment("path2", Parameter("matrixParamKey2", Some("matrixParamValue2")), Parameter("matrixParamKey3", None))), None, None)
    uri.matrixParametersOfLastSegment should equal(Seq(Parameter("matrixParamKey2", Some("matrixParamValue2")), Parameter("matrixParamKey3", None)))
  }

  it should "return `None` when the last segment is `StringSegment`" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), StringSegment("path2")), None, None)
    uri.matrixParametersOfLastSegment should equal(Seq.empty)
  }

  it should "return `None` when the last segment is empty" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1"))), EmptySegment), None, None)
    uri.matrixParametersOfLastSegment should equal(Seq.empty)
  }

  it should "return `None` when the path is empty" in {
    EmptyReference.matrixParametersOfLastSegment should equal(Seq.empty)
  }

  "`Uri.withPath`" should "change the path when provided a `Uri`" in {
    val path = AbsolutePath.option(Segment("path1"))
    val uri = Uri(None, None, path, None, None)
    EmptyReference.withPath(uri).path should equal(path)
  }

  it should "change the authority when provided an `Path`" in {
    val path = RootlessPath.option(Segment("path1"))
    val uri = Uri(None, None, path, None, None)
    uri.path should equal(path)
    uri.withPath(EmptyAbsolutePath).path.value should equal(EmptyAbsolutePath)
  }

  it should "remove the authority when provided nothing" in {
    val path = AbsolutePath.option(Segment("path1"))
    val uri = Uri(None, None, path, None, None)
    uri.path should equal(path)
    uri.withPath().path should equal(None)
  }

  "`Uri.appendSegment`" should "append a string segment" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.toString should equal("/path1")
    val uri2 = uri.appendSegment("path2")
    uri2.toString should equal("/path1/path2")
  }

  "`Uri.appendSegment`" should "append a matrix parameter segment" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1")), None, None)
    uri.toString should equal("/path1")
    val uri2 = uri.appendSegment("path2", "key", "value")
    uri2.toString should equal("/path1/path2;key=value")
  }

  "`Uri.appendMatrixParameter(String, String, String)`" should "append the key and value to the last segment (not already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path1"), EmptySegment), None, None)
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    uri.appendMatrixParameter("", "matrixParamKey1", "matrixParamValue1").path.value.segments.last should equal(Segment("", matrixParam1))
  }

  it should "append the parameter to the last segment (already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1")))), None, None)
    val matrixParam2 = Parameter("matrixParamKey2", "matrixParamValue2")
    uri.appendMatrixParameter("path1", "matrixParamKey2", "matrixParamValue2").path.value.segments.last should equal(MatrixParametersSegment("path1", Seq(Parameter("matrixParamKey1", Some("matrixParamValue1")), matrixParam2)))
  }

  it should "ignore the request when the path is empty" in {
    EmptyReference.appendMatrixParameter("path4", "matrixParamKey1", "matrixParamValue1").path should equal(None)
  }

  "`Uri.appendMatrixParameter(String, String, Option[String])`" should "append the parameter to the last segment (not already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path1"), EmptySegment), None, None)
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    uri.appendMatrixParameter("", matrixParam1.key, matrixParam1.value).path.value.segments.last should equal(Segment("", matrixParam1))
  }

  it should "append the parameter to the last segment (already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1")))), None, None)
    val matrixParam2 = Parameter("matrixParamKey2", "matrixParamValue2")
    uri.appendMatrixParameter("path1", matrixParam2.key, matrixParam2.value).path.value.segments.last should equal(Segment("path1", Seq(Parameter("matrixParamKey1", Some("matrixParamValue1")), matrixParam2)))
  }

  it should "ignore the request when the path is empty" in {
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    EmptyReference.appendMatrixParameter("", matrixParam1.key, matrixParam1.value).path should equal(None)
  }

  "`Uri.appendMatrixParameter(String, Parameter)`" should "append the parameter to the specified segment (not already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path1"), EmptySegment), None, None)
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    uri.appendMatrixParameter("", matrixParam1).path.value.segments.last should equal(Segment("", matrixParam1))
  }

  it should "append the parameter to the specified segment (already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(Segment("path1", Parameter("matrixParamKey1", Some("matrixParamValue1")))), None, None)
    val matrixParam2 = Parameter("matrixParamKey2", "matrixParamValue2")
    uri.appendMatrixParameter("path1", matrixParam2).path.value.segments.last should equal(MatrixParametersSegment("path1", Seq(Parameter("matrixParamKey1", Some("matrixParamValue1")), matrixParam2)))
  }

  it should "append the parameter to the specified segments" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path1"), EmptySegment, EmptySegment), None, None)
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    val uri2 = uri.appendMatrixParameter("", matrixParam1)
    uri2.path.value.segments should equal(Seq(StringSegment("path1"), Segment("", matrixParam1), Segment("", matrixParam1)))
  }

  it should "ignore the request when the path is empty" in {
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    EmptyReference.appendMatrixParameter("", matrixParam1).path should equal(None)
  }

  "`Uri.appendMatrixParameterToLastSegment(String, Any)`" should "append the parameter to the last segment (not already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path1"), EmptySegment), None, None)
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    uri.appendMatrixParameterToLastSegment(matrixParam1.key, matrixParam1.value).path.value.segments.last should equal(Segment("", matrixParam1))
  }

  it should "append the parameter to the last segment (already a MatrixParameterSegment)" in {
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", matrixParam1)), None, None)
    val matrixParam2 = Parameter("matrixParamKey2", "matrixParamValue2")
    uri.appendMatrixParameterToLastSegment(matrixParam2.key, matrixParam2.value).path.value.segments.last should equal(MatrixParametersSegment("path1", Seq(matrixParam1, matrixParam2)))
  }

  it should "ignore the request when there is no path" in {
    EmptyReference.appendMatrixParameterToLastSegment("matrixParamKey1", "matrixParamValue1").path should equal(None)
  }

  "`Uri.appendMatrixParameterToLastSegment(Parameter)`" should "append the parameter to the last segment (not already a MatrixParameterSegment)" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path1"), EmptySegment), None, None)
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    uri.appendMatrixParameterToLastSegment(matrixParam1).path.value.segments.last should equal(Segment("", matrixParam1))
  }

  it should "append the parameter to the last segment (already a MatrixParameterSegment)" in {
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path1", matrixParam1)), None, None)
    val matrixParam2 = Parameter("matrixParamKey2", "matrixParamValue2")
    uri.appendMatrixParameterToLastSegment(matrixParam2).path.value.segments.last should equal(MatrixParametersSegment("path1", Seq(matrixParam1, matrixParam2)))
  }

  it should "ignore the request when there is no path" in {
    val matrixParam1 = Parameter("matrixParamKey1", "matrixParamValue1")
    EmptyReference.appendMatrixParameterToLastSegment(matrixParam1).path should equal(None)
  }

  "`Uri.pathToString` and therefore `Path.toString`" should "work with an abolute path with a single string segment" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path")), None, None)
    uri.pathToString should equal("/path")
  }

  it should "work with an abolute path with a single empty segment" in {
    val uri = Uri(None, None, AbsolutePath.option(EmptySegment), None, None)
    uri.pathToString should equal("/")
  }

  it should "work with an abolute path with a single matrix parameter segment with key and value" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path", Parameter("matrixParamKey", Some("matrixParamValue")))), None, None)
    uri.pathToString should equal("/path;matrixParamKey=matrixParamValue")
  }

  it should "work with an abolute path with a single matrix parameter segment with key and empty value" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path", Parameter("matrixParamKey", Some("")))), None, None)
    uri.pathToString should equal("/path;matrixParamKey=")
  }

  it should "work with an abolute path with a single matrix parameter segment with key and no value" in {
    val uri = Uri(None, None, AbsolutePath.option(MatrixParametersSegment("path", Parameter("matrixParamKey", None))), None, None)
    uri.pathToString should equal("/path;matrixParamKey")
  }

  it should "work with an abolute path with multiple segments" in {
    val uri = Uri(None, None, AbsolutePath.option(StringSegment("path"), EmptySegment, MatrixParametersSegment("path", Parameter("matrixParamKey", Some("matrixParamValue"))), MatrixParametersSegment("path", Parameter("matrixParamKey", Some(""))), MatrixParametersSegment("path", Parameter("matrixParamKey", None)), EmptySegment), None, None)
    uri.pathToString should equal("/path//path;matrixParamKey=matrixParamValue/path;matrixParamKey=/path;matrixParamKey/")
  }

  it should "work with a rootless path with a single string segment" in {
    val uri = Uri(None, None, RootlessPath.option(StringSegment("path")), None, None)
    uri.pathToString should equal("path")
  }

  it should "work with a rootless path with a single empty segment" in {
    val uri = Uri(None, None, RootlessPath.option(EmptySegment), None, None)
    uri.pathToString should equal("")
  }

  it should "work with a rootless path with a single matrix parameter segment with key and value" in {
    val uri = Uri(None, None, RootlessPath.option(MatrixParametersSegment("path", Parameter("matrixParamKey", Some("matrixParamValue")))), None, None)
    uri.pathToString should equal("path;matrixParamKey=matrixParamValue")
  }

  it should "work with a rootless path with a single matrix parameter segment with key and empty value" in {
    val uri = Uri(None, None, RootlessPath.option(MatrixParametersSegment("path", Parameter("matrixParamKey", Some("")))), None, None)
    uri.pathToString should equal("path;matrixParamKey=")
  }

  it should "work with a rootless path with a single matrix parameter segment with key and no value" in {
    val uri = Uri(None, None, RootlessPath.option(MatrixParametersSegment("path", Parameter("matrixParamKey", None))), None, None)
    uri.pathToString should equal("path;matrixParamKey")
  }

  it should "work with a rootless path with multiple segments" in {
    val uri = Uri(None, None, RootlessPath.option(StringSegment("path"), EmptySegment, MatrixParametersSegment("path", Parameter("matrixParamKey", Some("matrixParamValue"))), MatrixParametersSegment("path", Parameter("matrixParamKey", Some(""))), MatrixParametersSegment("path", Parameter("matrixParamKey", None)), EmptySegment), None, None)
    uri.pathToString should equal("path//path;matrixParamKey=matrixParamValue/path;matrixParamKey=/path;matrixParamKey/")
  }

  it should "work without a path" in {
    EmptyReference.pathToString should equal("")
  }

  "`Path.appendSegment`" should "succeed" in {
    val segment1 = Segment("path1")
    val segment2 = Segment("path2")
    val segment3 = Segment("path3")
    AbsolutePath(segment1, segment2).appendSegment(segment3).segments should equal(Seq(segment1, segment2, segment3))
  }

  "`Path.appendSegments`" should "succeed when passed `Seq[Segment]`" in {
    val segment1 = Segment("path1")
    val segment2 = Segment("path2")
    val segment3 = Segment("path3")
    RootlessPath(segment1).appendSegments(Seq(segment2, segment3)).segments should equal(Seq(segment1, segment2, segment3))
  }

  it should "succeed when passed `Segment*`" in {
    val segment1 = Segment("path1")
    val segment2 = Segment("path2")
    val segment3 = Segment("path3")
    RootlessPath(segment1).appendSegments(segment2, segment3).segments should equal(Seq(segment1, segment2, segment3))
  }

  it should "succeed when passed `Path`" in {
    val segment1 = Segment("path1")
    val segment2 = Segment("path2")
    val segment3 = Segment("path3")
    AbsolutePath(segment1).appendSegments(AbsolutePath(Seq(segment2, segment3))).segments should equal(Seq(segment1, segment2, segment3))
  }

  "`AbsolutePath.apply`" should "fail when segments is `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      AbsolutePath(null: Seq[Segment])
    }
  }

  it should "return `EmptyAbsolutePath` when segments is empty" in {
    AbsolutePath(Seq.empty) should equal(EmptyAbsolutePath)
  }

  it should "return `EmptyAbsolutePath` when segments is nothing" in {
    AbsolutePath() should equal(EmptyAbsolutePath)
  }

  it should "return `EmptyAbsolutePath` when segments is `EmptySegment`" in {
    AbsolutePath(EmptySegment) should equal(EmptyAbsolutePath)
  }

  "`AbsolutePath.option`" should "return `None` when segments is `null`" in {
    AbsolutePath.option(null: Seq[Segment]) should equal(None)
  }

  it should "return `None` when segments is empty" in {
    AbsolutePath.option(Seq.empty) should equal(None)
  }

  it should "return `None` when segments is nothing" in {
    AbsolutePath.option() should equal(None)
  }

  "`RootlessPath.apply`" should "fail when segments is `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      RootlessPath(null: Seq[Segment])
    }
  }

  it should "fail when segments is empty" in {
    an [IllegalArgumentException] should be thrownBy {
      RootlessPath(Seq.empty)
    }
  }

  it should "fail when segments is nothing" in {
    an [IllegalArgumentException] should be thrownBy {
      RootlessPath()
    }
  }

  it should "fail when the only segment is empty" in {
    an [IllegalArgumentException] should be thrownBy {
      RootlessPath(EmptySegment)
    }
  }

  it should "fail when the first segment is empty" in {
    an [IllegalArgumentException] should be thrownBy {
      RootlessPath(EmptySegment, Segment("path"))
    }
  }

  "`RootlessPath.option`" should "return `None` when segments is `null`" in {
    RootlessPath.option(null: Seq[Segment]) should equal(None)
  }

  it should "return `None` when segments is empty" in {
    RootlessPath.option(Seq.empty) should equal(None)
  }

  it should "return `None` when segments is nothing" in {
    RootlessPath.option() should equal(None)
  }

  "`StringSegment.mapSegment`" should "succeed when replacing the segment" in {
    StringSegment("path1").mapSegment(_ => "otherPath1") should equal(StringSegment("otherPath1"))
  }

  it should "succeed when manipulating the segment" in {
    StringSegment("path1").mapSegment(_.init) should equal(StringSegment("path"))
  }

  "`StringSegment.copy`" should "succeed" in {
    StringSegment("path1").copy("otherPath1") should equal(StringSegment("otherPath1"))
  }

  it should "fail when passed `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      StringSegment("path1").copy(null)
    }
  }

  "`MatrixParametersSegment.mapSegment`" should "succeed when replacing the segment" in {
    MatrixParametersSegment("path1").mapSegment(_ => "otherPath1") should equal(MatrixParametersSegment("otherPath1"))
  }

  it should "succeed when manipulating the segment" in {
    MatrixParametersSegment("path1").mapSegment(_.init) should equal(MatrixParametersSegment("path"))
  }

  "`MatrixParametersSegment.copy`" should "succeed with segment and parameters" in {
    val parameter1 = Parameter("matrixParamKey1", "matrixParamValue1")
    val segment = MatrixParametersSegment("path1").copy("otherPath1", Seq(parameter1))
    segment.segment should equal("otherPath1")
    segment.parameters should equal(Seq(parameter1))
  }

  it should "succeed with segment, and without parameters" in {
    val segment = MatrixParametersSegment("path1").copy("otherPath1")
    segment.segment should equal("otherPath1")
    segment.parameters should equal(Seq.empty)
  }

  it should "succeed with parameters, and without segment" in {
    val parameter1 = Parameter("matrixParamKey1", "matrixParamValue1")
    val segment = MatrixParametersSegment("path1").copy(parameters = Seq(parameter1))
    segment.segment should equal("path1")
    segment.parameters should equal(Seq(parameter1))
  }

  it should "fail when passed `null` segment" in {
    an [IllegalArgumentException] should be thrownBy {
      MatrixParametersSegment("path1").copy(null)
    }
  }

  "`StringSegment.apply`" should "fail when segment is `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      StringSegment(null)
    }
  }

  it should "return `EmptySegment`" in {
    StringSegment("") should equal(EmptySegment)
  }

  "`MatrixParametersSegment.apply`" should "fail when segment is `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      MatrixParametersSegment(null, Seq.empty)
    }
  }

  it should "fail when parameters is `null`" in {
    an [IllegalArgumentException] should be thrownBy {
      MatrixParametersSegment("", null: Seq[Parameter])
    }
  }
}
