# Deploy to Render

This repository is ready for a Docker-based Render deployment.

## Files added

- `Dockerfile` installs R, system libraries, and the Shiny dependencies.
- `start-render.R` starts Shiny on `0.0.0.0:$PORT`, which Render requires.
- `render-start.sh` seeds CSV data and photos into the persistent disk on first deploy.
- `render.yaml` defines the Render web service, disk, environment variables, and `app.csbblr.online`.
- `.dockerignore` keeps screenshots, local R history, and rsconnect metadata out of the image.

## Deploy

1. Push this repo to GitHub/GitLab/Bitbucket.
2. In Render, create a new Blueprint from the repo, or create a Web Service and select Docker.
3. Render will build the Dockerfile and run the service using the command in the image.
4. On the free plan, app data is stored in the service filesystem and can reset when Render redeploys or restarts the service.

## Connect `app.csbblr.online`

The Blueprint includes:

```yaml
domains:
  - app.csbblr.online
```

After the service exists in Render:

1. Open the service in the Render Dashboard.
2. Go to Settings > Custom Domains and confirm `app.csbblr.online` is listed.
3. Add this DNS record at your domain/DNS provider:

```text
Type:  CNAME
Name:  app
Value: student-performance-analyzer.onrender.com
TTL:   Auto or 300
```

If Render gives your service a different `onrender.com` hostname, use that exact value instead.

4. Return to Render and click Verify for the custom domain.
5. Render automatically provisions TLS and redirects HTTP to HTTPS after verification.

Final URL:

```text
https://app.csbblr.online
```

## Notes

- The configured region is `singapore`.
- The configured instance plan is `free`.
- Uploaded photos and CSV changes are not guaranteed to persist on the free plan. Upgrade to a paid instance with a persistent disk when you need durable production data.
