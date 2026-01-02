# CopyCommander2, REST-API

This is the CopyCommander2 Rest API Documentation.

CopyCommander REST API only supports JSON as data format. To see how to enable / setup the REST functionality visit the [rest api](how_to_use.md#rest-api) section in the how_to_use document.

There are 2 kinds of paths supported by CopyCommander2.

#### Normal paths:
Normal paths are always available and can be used to get status, start jobs, ...

#### Zombie paths:
Zombie paths are used to control the application (visible to the user), to enable these paths you need to pass in a extra command line paramter.

## List of available methods / paths
| Method | Overview | Kind | Description
| --- | --- | --- | --- |
| GET | [/API/status](#get-apistatus) | normal | get current state of application
| GET | [/API/view/list](#get-apiviewlist) | normal | get list of content from a specific view
| POST | [/API/jobs](#post-apijobs) | normal | start a job 
| POST | [/API/shutdown](#post-apishutdown) | normal | shutdown CopyCommander2
| POST | [/API/zombie/setdir](#post-apizombiesetdir) | zombie | set directory path for left or right view

## GET /API/status

Returns the current status of the application.

### Request

**Method:** GET  
**Path:** `/API/status`  
**Content-Type:** Not applicable

### Query Parameters

none

### Response

**Content-Type:** `application/json`

### Response Schema

| Field | Type | Description |
|-------|------|-------------|
| `State` | integer | Current application state (0 = idle, 1 = busy) |
| `JobQueueCount` | integer | Number of jobs currently in the queue |
| `LeftDir` | string | Path to the left directory |
| `RightDir` | string | Path to the right directory |

### Example Request

```
GET /API/status
```

### Example Response

```json
{
    "State": 0,
    "JobQueueCount": 3,
    "LeftDir": "C:\\Users\\john\\Documents\\Source",
    "RightDir": "C:\\Users\\john\\Documents\\Backup"
}
```
## GET /API/view/list

Returns the list of files and folders in the specified view.

### Request

**Method:** GET  
**Path:** `/API/view/list`  
**Content-Type:** Not applicable

### Query Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `view` | string | No | View to list ("left" or "right"). Defaults to "left" if not specified |

### Response

**Content-Type:** `application/json`

### Response Schema

| Field | Type | Description |
|-------|------|-------------|
| `dir` | string | Absolute path of the current directory |
| `folders` | array | Array of folder names in the view |
| `files` | array | Array of file names in the view |

### Example Request

```
GET /API/view/list?view=right
```

### Example Response

```json
{
    "folders": [
        "Documents",
        "Pictures",
        "Videos"
    ],
    "files": [
        "readme.txt",
        "config.json",
        "backup.zip"
    ]
}
```

## POST /API/jobs

Creates a new job to copy, move, or delete files/folders.

### Request

**Method:** POST  
**Path:** `/API/jobs`  
**Content-Type:** `application/json`

### Request Schema

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `operation` | string | Yes | Type of operation: "copy", "move", "delete" or "delete_empty_subfolders" |
| `source` | string | Yes | Source file or directory path |
| `target` | string | Conditional | Target directory path (required for "copy" and "move", ignored for "delete" or "delete_empty_subfolders") |

### Example Requests

**Copy operation:**
```json
{
    "operation": "copy",
    "source": "C:\\Users\\john\\Documents\\file.txt",
    "target": "C:\\Users\\john\\Backup"
}
```

**Move operation:**
```json
{
    "operation": "move",
    "source": "C:\\Users\\john\\Documents\\folder",
    "target": "C:\\Users\\john\\Archive"
}
```

**Delete operation:**
```json
{
    "operation": "delete",
    "source": "C:\\Users\\john\\Documents\\temp.txt"
}
```

### Response

**HTTP Status Codes:**
- `201 Created` - Job successfully created and queued
- `400 Bad Request` - Invalid operation type or missing required fields
- `404 Not Found` - Source file/directory does not exist
- `422 Unprocessable Entity` - Invalid paths or target directory does not exist

**Content-Type:** `application/json`

### Response Schema

| Field | Type | Description |
|-------|------|-------------|
| `status` | string | Current job status ("queued") |

### Example Response

```json
{
    "status": "queued"
}
```

## POST /API/shutdown

Shuts down the CopyCommander2 application.

### Request

**Method:** POST  
**Path:** `/API/shutdown`  
**Content-Type:** `application/json`

### Request Schema

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `skipJobs` | boolean | No | Whether to skip currently running jobs (default: false) |

### Example Request

```json
{
    "skipJobs": true
}
```

### Response

**HTTP Status Codes:**
- `200 OK` - Application shutdown initiated successfully
- `403 Forbidden` - Shutdown not allowed (jobs are running and skipJobs is false)
  
## POST /API/zombie/setdir

Sets the directory path for either the left or right view.

### Request

**Method:** POST  
**Path:** `/API/zombie/setdir`  
**Content-Type:** `application/json`

### Request Schema

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `view` | string | No | Must be either "left" or "right", default (left) |
| `dir` | string | Yes | Directory path to set |

### Example Request

```json
{
    "view": "left",
    "dir": "C:\\Users\\john\\Documents\\NewSource"
}
```

### Response

**HTTP Status Codes:**
- `204 No Content` - Directory successfully set
- `422 Unprocessable Entity` - Invalid request data / target dir, does not exist.
