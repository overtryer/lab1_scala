import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import javax.crypto.spec.SecretKeySpec

class CreateHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    ctx.logger.info("Начало создания хранилища")
    val roots = java.nio.file.FileSystems.getDefault.getRootDirectories.asScala.toList

    ctx.ui.println("Доступные устройства:")
    roots.zipWithIndex.foreach { case (root, idx) => ctx.ui.println(s"  ${idx + 1}. $root") }
    ctx.ui.print("Выберите устройство (номер): ")
    val driveIdx = ctx.ui.readLineTrimmed().toIntOption.getOrElse(0) - 1

    if driveIdx < 0 || driveIdx >= roots.size then
      ctx.ui.println("Неверный выбор устройства.")
      return state

    val selectedRoot = roots(driveIdx)

    ctx.ui.println("Введите имя файла хранилища (например, backup.store):")
    val fileName = ctx.ui.readLineTrimmed()
    if fileName.isEmpty then return state
    val path = selectedRoot.resolve(fileName)

    ctx.ui.println("Введите название хранилища:")
    val storeName = ctx.ui.readLineTrimmed()
    if storeName.isEmpty then return state

    ctx.ui.println("Использовать шифрование? (y/n):")
    val isEncrypted = ctx.ui.readLineTrimmed().toLowerCase == "y"

    var keyOpt: Option[SecretKeySpec] = None
    if isEncrypted then
      ctx.ui.println("Способ хранения ключа: 1 - в файл, 2 - показать пользователю")
      val choice = ctx.ui.readLineTrimmed()
      val keyStr = Crypto.generateKey()
      keyOpt = Crypto.parseKey(keyStr.value).toOption

      if choice == "1" then
        val keyDir = Paths.get(ctx.config.keyDirectory)
        BusinessLogic.saveKeyToFile(keyDir, s"${path.getFileName}.key", keyStr.value) match
          case Right(keyFile) =>
            ctx.ui.println(s"Ключ сохранен в: ${keyFile.toAbsolutePath}")
            ctx.logger.info(s"Ключ шифрования сохранен в файл: $keyFile")
          case Left(err) =>
            ctx.ui.println(s"Ошибка сохранения ключа: ${err.message}")
            ctx.logger.error(s"Не удалось сохранить ключ: ${err.message}")
            return state
      else
        ctx.ui.println(s"КЛЮЧ: ${keyStr.value}")
        ctx.ui.println("Подтвердите, что ключ сохранен (y):")
        if ctx.ui.readLineTrimmed().toLowerCase != "y" then
          ctx.logger.info("Создание хранилища отменено пользователем")
          return state

    ctx.ui.println("Добавьте первоначальные источники (пути к папкам). Оставьте пустым для завершения:")
    var sourcesList = List.empty[Source]
    var adding = true
    while adding do
      ctx.ui.print(s"Путь к источнику ${sourcesList.size + 1}: ")
      val p = ctx.ui.readLineTrimmed()
      if p.isEmpty then adding = false
      else
        val newId = SourceId(sourcesList.map(_.id.value).maxOption.getOrElse(0L) + 1L)
        sourcesList = sourcesList :+ Source(newId, Paths.get(p))
    
    BusinessLogic.createStorage(ctx.storage, path, storeName, isEncrypted, sourcesList, keyOpt) match
      case Right(newSession) =>
        ctx.ui.println(s"Хранилище создано по пути: ${path.toAbsolutePath}")
        ctx.logger.info(s"Создано хранилище: $path, Источников: ${sourcesList.size}, Шифрование: $isEncrypted")
        AppState.Opened(newSession)
      case Left(err) =>
        ctx.ui.println(s"Ошибка: ${err.message}")
        ctx.logger.error(s"Ошибка создания хранилища: ${err.message}")
        state