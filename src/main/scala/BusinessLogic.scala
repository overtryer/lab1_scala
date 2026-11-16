import java.time.Instant
import java.nio.file.{Files, Path}
import javax.crypto.spec.SecretKeySpec
import scala.util.Try

object BusinessLogic:

  // InfoHandler
  def infoLines(session: StorageSession): List[String] =
    val lastBackup = session.sources.flatMap(_.lastBackup).maxOption
    List(
      s"Хранилище: ${session.name}",
      s"Шифрование: ${if session.encrypted then "включено" else "выключено"}",
      s"Источников: ${session.sources.size}",
      s"Последняя резервная копия: ${lastBackup.map(formatMinute).getOrElse("—")}",
      s"Версий: ${session.versions.size}",
      s"Файлов в базе: ${session.manifest.files.size}"
    )

  // VersionsHandler
  def versionsLines(versions: List[VersionRecord]): List[String] =
    if versions.isEmpty then List("Версий пока нет.")
    else
      "Доступные версии:" :: versions.map { v =>
        val dateStr = v.createdAt.toString.take(19).replace("T", " ")
        val filesCount = v.manifest.files.size
        // Используем .formatted() вместо f-интерполятора во избежание warning-ов
        s"Версия ${v.id.value.toInt.formatted("%3d")} | $dateStr | Файлов: ${filesCount.formatted("%3d")} | +${v.added} ~${v.modified} -${v.removed}"
      }

  // ScanHandler
  final case class ScanReport(stats: ChangeStats, problems: List[Change])

  def scan(session: StorageSession, scanner: Scanner): ScanReport =
    val result = scanner.scan(session.sources)
    val changes = Differ.diff(session.manifest, result, session.sources)
    val stats = ChangeStats.from(changes)
    ScanReport(stats, result.problems)

  def scanLines(report: ScanReport): List[String] =
    if report.stats.isEmpty && report.problems.isEmpty then
      List("Изменений не обнаружено.")
    else
      List(s"Изменения: +${report.stats.added}, ~${report.stats.modified}, -${report.stats.removed}, пер.: ${report.stats.renamed}")

  // ReloadHandler
  def reload(configPath: Path): Either[AppError, (AppConfig, Scanner)] =
    ConfigLoader.load(configPath).map(cfg => (cfg, new Scanner(cfg.hashAlgorithm)))

  // OpenHandler
  def openSession(storage: StorageService, path: Path): Either[AppError, StorageSession] =
    storage.readSession(path, None)

  def openEncryptedSession(storage: StorageService, path: Path, keyInput: String): Either[AppError, StorageSession] =
    for
      keySpec <- Crypto.parseKey(keyInput)
      session <- storage.readSession(path, Some(keySpec))
    yield session

  // CreateHandler
  def saveKeyToFile(keyDir: Path, fileName: String, keyStr: String): Either[AppError, Path] =
    Try {
      Files.createDirectories(keyDir)
      val keyFile = keyDir.resolve(fileName)
      Files.writeString(keyFile, keyStr)
      keyFile
    }.toEither.left.map(e => AppError.StorageIoError(s"Не удалось сохранить ключ: ${e.getMessage}"))

  def createStorage(
                     storage: StorageService,
                     path: Path,
                     name: String,
                     encrypted: Boolean,
                     sources: List[Source],
                     keyOpt: Option[SecretKeySpec]
                   ): Either[AppError, StorageSession] =
    val newSession = StorageSession(
      storageFile = path, name = name, encrypted = encrypted,
      manifest = Manifest(VersionId(0), Instant.now(), Map.empty),
      versions = Nil, sources = sources, key = keyOpt
    )
    storage.createNew(path, newSession, keyOpt).map(_ => newSession)

  // SourcesHandler
  def addSource(session: StorageSession, path: Path): Either[AppError, StorageSession] =
    val newId = SourceId(session.sources.map(_.id.value).maxOption.getOrElse(0L) + 1L)
    Right(session.copy(sources = session.sources :+ Source(newId, path)))

  def toggleSource(session: StorageSession, index: Int): Either[AppError, StorageSession] =
    session.sources.lift(index - 1) match
      case None => Left(AppError.InvalidSourceIndex(index))
      case Some(src) =>
        val updated = session.sources.map(s => if s.id == src.id then s.copy(enabled = !s.enabled) else s)
        Right(session.copy(sources = updated))

  def removeSource(session: StorageSession, index: Int): Either[AppError, StorageSession] =
    session.sources.lift(index - 1) match
      case None => Left(AppError.InvalidSourceIndex(index))
      case Some(src) =>
        val updated = session.sources.filterNot(_.id == src.id)
        Right(session.copy(sources = updated))

  def editSource(session: StorageSession, index: Int, recursive: Boolean, exclusions: List[Glob]): Either[AppError, StorageSession] =
    session.sources.lift(index - 1) match
      case None => Left(AppError.InvalidSourceIndex(index))
      case Some(src) =>
        val updated = session.sources.map(s => if s.id == src.id then s.copy(recursive = recursive, exclusions = exclusions) else s)
        Right(session.copy(sources = updated))

  // BackupHandler
  final case class BackupPlan(
                               newSession: StorageSession,
                               filesToAdd: Map[Hash, Path],
                               stats: ChangeStats
                             )

  def getUsableSpace(path: Path): Either[AppError, Long] =
    Try(Files.getFileStore(path).getUsableSpace).toEither
      .left.map(e => AppError.StorageIoError(s"Не удалось получить размер свободного места: ${e.getMessage}"))

  def planBackup(session: StorageSession, changes: List[Change], problems: List[Change]): BackupPlan =
    val stats = ChangeStats.from(changes)

    val filesToAdd = changes.collect {
      case Change.Added(entry) => entry
      case Change.Modified(_, current) => current
    }.flatMap { entry =>
      session.sources.find(_.id == entry.sourceId).map(src => (entry.hash, src.path.resolve(entry.relPath.value)))
    }.toMap

    val prevFiles = session.manifest.files
    val removedKeys = changes.collect { case Change.Removed(e) => (e.sourceId, e.relPath) }.toSet
    val addedEntries = changes.collect {
      case Change.Added(e) => (e.sourceId, e.relPath) -> e
      case Change.Renamed(_, curr) => (curr.sourceId, curr.relPath) -> curr
    }.toMap
    val modifiedEntries = changes.collect { case Change.Modified(_, curr) => (curr.sourceId, curr.relPath) -> curr }.toMap

    val newFilesMap = (prevFiles -- removedKeys) ++ addedEntries ++ modifiedEntries
    val newVersionId = VersionId(session.manifest.version.value + 1)
    val newManifest = Manifest(newVersionId, Instant.now(), newFilesMap)

    val newRecord = VersionRecord(
      id = newVersionId, createdAt = newManifest.createdAt,
      added = stats.added + stats.renamed, modified = stats.modified, removed = stats.removed,
      result = if problems.isEmpty then BackupOutcome.Success else BackupOutcome.Partial,
      comment = None, manifest = newManifest
    )

    val now = Instant.now()
    val unavailableIds = problems.collect { case Change.SourceUnavailable(sid, _) => sid }.toSet
    val updatedSources = session.sources.map { s =>
      if s.enabled && !unavailableIds.contains(s.id) then s.copy(lastBackup = Some(now)) else s
    }

    val newSession = session.copy(
      manifest = newManifest,
      versions = newRecord +: session.versions,
      sources = updatedSources
    )
    BackupPlan(newSession, filesToAdd, stats)

  // RestoreHandler
  def resolveManifest(session: StorageSession, choice: String): Either[AppError, Manifest] =
    if choice == "0" then Right(session.manifest)
    else
      choice.toIntOption
        .toRight(AppError.VersionNotFound(choice))
        .flatMap(id => session.versions.find(_.id.value == id).map(_.manifest)
          .toRight(AppError.VersionNotFound(choice)))

  // VerifyHandler
  def verifyFiles(storage: StorageService, session: StorageSession, scanner: Scanner): List[String] =
    session.manifest.files.values.toList.flatMap { entry =>
      Try(Files.createTempFile("verify_", ".tmp")).toEither match
        case Left(e) => Some(s"Ошибка создания временного файла: ${e.getMessage}")
        case Right(tempDest) =>
          val result = storage.extractFile(session.storageFile, entry.hash, tempDest, session.key) match
            case Left(err) => Some(s"Повреждение: ${entry.relPath.value} - ${err.message}")
            case Right(_) =>
              Try(scanner.hashFile(tempDest)).toEither match
                case Left(e) => Some(s"Ошибка проверки: ${entry.relPath.value} - ${e.getMessage}")
                case Right(actualHash) =>
                  if actualHash != entry.hash then Some(s"Повреждение: ${entry.relPath.value} - хеш не совпадает")
                  else None
          
          try Files.deleteIfExists(tempDest) catch { case _: Exception => () }
          result
    }
  
  private def formatMinute(i: Instant): String = i.toString.take(16).replace("T", " ")